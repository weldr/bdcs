-- Copyright (C) 2017 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Build.NPM(rebuildNPM)
 where

import           Control.Exception.Lifted(bracket)
import           Control.Monad(forM_, void, when)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.State(execStateT)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource)
import           Data.Bifunctor(bimap)
import           Data.Conduit((.|), runConduit)
import qualified Data.Text as T
import           Data.Time.Clock(getCurrentTime)
import           Database.Esqueleto
import           GI.OSTree(IsRepo)
import           Shelly(shelly, cp_r, fromText)
import           System.Directory(createDirectory, createDirectoryIfMissing, listDirectory, removePathForcibly)
import           System.FilePath((</>), joinPath)
import           System.IO.Temp(createTempDirectory)
import           System.Posix.Files(createSymbolicLink)

import BDCS.Builds(insertBuild, insertBuildKeyValue)
import BDCS.CS(commit, commitContents, commitContentToFile, filesToObjectsC, storeDirectory, withTransaction)
import BDCS.DB
import BDCS.Files(associateFilesWithBuild, insertFiles, sourceIdToFiles)
import BDCS.KeyType
import BDCS.NPM.SemVer(SemVer, SemVerRangeSet, parseSemVer, parseSemVerRangeSet, satisfies, toText)
import Export.Directory(directorySink)

rebuildNPM :: (IsRepo a, MonadBaseControl IO m, MonadIO m, MonadError String m, MonadResource m) => a -> Key Sources -> SqlPersistT m [Key Builds]
rebuildNPM repo sourceId = do
    -- get the name and version for this source
    (name, version) <- getNameVer

    -- figure out what sources satisfy the dependencies for this package
    -- Each list element represents one of the dependencies, within those each
    -- element is a source ID that satisfies the dependencies. sequence the whole thing
    -- to get all of the possible combinations that satisfy all dependencies.
    dependencies <- sequence <$> getDeps

    -- run with a temp directory for the export
    bracket (liftIO $ createTempDirectory "." "npm-export") (liftIO . removePathForcibly) $ \exportPath -> do
        -- start by copying the files for the source to a temp directory
        runConduit $ sourceIdToFiles sourceId .| filesToObjectsC repo .| directorySink exportPath

        -- For each dependency list, create a new build
        mapM (relink exportPath (name, version)) dependencies
 where
    getDeps :: (MonadIO m, MonadError String m) => SqlPersistT m [[(T.Text, SemVer)]]
    getDeps = do
        -- fetch the list of dependencies
        -- the dependencies for a given source are stored as key/vals, k="dependency", v=package name, e=version expression
        kvs <- select $ from $ \(kv `InnerJoin` skv) -> do
               on     $ kv ^. KeyValId ==. skv ^. SourceKeyValuesKey_val_id
               where_ $ skv ^. SourceKeyValuesSource_id ==. val sourceId &&.
                        kv ^. KeyValKey_value ==. val (TextKey "dependency")
               return (kv ^. KeyValVal_value, kv ^. KeyValExt_value)
        depnames <- mapM (unpackName . fst) kvs
        depvers <- mapM (unpackVersion . snd) kvs

        mapM getOneDep $ zip depnames depvers
     where
        unpackName name = maybe (throwError "Invalid dependency name") return $ unValue name

        unpackVersion ver = do
            unmaybe <- maybe (throwError "Invalid dependency version") return $ unValue ver
            either (throwError . show) return $ parseSemVerRangeSet unmaybe

    getOneDep :: (MonadIO m, MonadError String m) => (T.Text, SemVerRangeSet) -> SqlPersistT m [(T.Text, SemVer)]
    getOneDep (name, range) = do
        -- Get all npm Sources records that match the name
        sources <- select $ from $ \(p `InnerJoin` s `InnerJoin` skv `InnerJoin` kv) -> do
                   on     $ kv ^. KeyValId ==. skv ^. SourceKeyValuesKey_val_id
                   on     $ s ^. SourcesId ==. skv ^. SourceKeyValuesSource_id
                   on     $ p ^. ProjectsId ==. s ^. SourcesProject_id
                   where_ $ kv ^. KeyValKey_value ==. val (TextKey "npm") &&.
                            p ^. ProjectsName ==. val name
                   return $ s ^. SourcesVersion

        -- if nothing is found, that's an error
        when (null sources) $ throwError $ "Unable to satisfy dependency for " ++ show name ++ " " ++ show range

        -- Parse the versions into SemVers
        versions <- mapM unpackVersion sources

        let filteredVersions = filter (`satisfies` range) versions
        return $ zip (repeat name) filteredVersions

     where
        unpackVersion ver = either (throwError . show) return $ parseSemVer $ unValue ver

    getNameVer :: (MonadIO m, MonadError String m) => SqlPersistT m (T.Text, T.Text)
    getNameVer = do
        nv <- select $ from $ \(sources `InnerJoin` projects) -> do
              on     $ sources ^. SourcesProject_id ==. projects ^. ProjectsId
              where_ $ sources ^. SourcesId ==. val sourceId
              limit 1
              return   (projects ^. ProjectsName, sources ^. SourcesVersion)

        when (null nv) $ throwError $ "No such source id " ++ show sourceId

        return $ bimap unValue unValue $ head nv

    relink :: (MonadBaseControl IO m, MonadIO m) => FilePath -> (T.Text, T.Text) -> [(T.Text, SemVer)] -> SqlPersistT m (Key Builds)
    relink exportPath (name, ver) depList = do
        -- Create a temp directory for the build
        files <- bracket (liftIO $ createTempDirectory "." "npm-build") (liftIO . removePathForcibly) $ \importPath -> do
            -- Create a directory for this module in /usr/lib/node_modules
            -- NB: In order to allow multiple versions of an npm module to be included in the same export,
            -- the /usr/lib/node_modules name is <module-name>@<module-version> instead of just <module-name>,
            -- and none of the bin or man symlinks are installed to /usr/bin and /usr/share/man. It's up to the
            -- export to determine which modules need to be accessible system-wide and to create the bin and man
            -- symlinks and the /usr/lib/node_modules/<module-name> directory.
            let module_dir = importPath </> "usr" </> "lib" </> "node_modules" </> T.unpack (T.concat [name, "@", ver])

            liftIO $ do
                createDirectoryIfMissing True module_dir

                -- Copy everything from the Source into the module directory
                -- convert the paths into the other kind of FilePath so shelly can use them
                filelist <- map (fromText . T.pack . (exportPath </>)) <$> listDirectory exportPath
                let module_fp = fromText $ T.pack module_dir
                shelly $ mapM_ (`cp_r` module_fp) filelist

                -- Create the node_modules directory for the module's dependencies
                createDirectory $ module_dir </> "node_modules"

                -- For each for the dependencies, create a symlink from the /usr/lib/node_modules/<name>@<version> directory
                -- to this module's node_modules directory.
                mapM_ (createDepLink module_dir) depList

                -- Import this directory into the content store
                commitChecksum <- withTransaction repo $ \r -> do
                    f <- storeDirectory r importPath
                    commit r f (T.concat ["Import of build of NPM package ", name, "@", ver]) Nothing

                -- Get the list of files we just imported
                checksums <- execStateT (commitContents repo commitChecksum) []

                -- Convert the commit contents to Files records
                mapM (commitContentToFile importPath) checksums

        -- Create a build and add the files to it
        createBuild files
     where
        createDepLink module_dir (depname, depver) = let
            verstr = toText depver
            source = joinPath ["/", "usr", "lib", "node_modules", T.unpack (T.concat [depname, "@", verstr])]
            dest   = joinPath [module_dir, "node_modules", T.unpack depname]
         in
            createSymbolicLink source dest

        createBuild :: MonadIO m => [Files] -> SqlPersistT m (Key Builds)
        createBuild files = do
            buildTime <- liftIO getCurrentTime

            -- There is no equivalent to epoch or release in npm, so use 0 and ""
            let epoch = 0
            let release = ""
            -- FIXME there are some npm packages that are arch-specific but for now ignore those
            let arch = "noarch"
            -- FIXME changelog?
            let changelog = ""
            -- FIXME ??
            let build_config_ref = "BUILD_CONFIG_REF"
            let build_env_ref = "BUILD_ENV_REF"

            buildId <- insertBuild $ Builds sourceId epoch release arch buildTime changelog build_config_ref build_env_ref
            fids <- insertFiles files
            void $ associateFilesWithBuild fids buildId

            -- Record the exact-version dependencies used for this build
            forM_ depList $ \(n, v) -> insertBuildKeyValue (TextKey "dependency") n (Just $ toText v) buildId

            return buildId
