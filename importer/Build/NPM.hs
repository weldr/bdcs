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
{-# LANGUAGE RecordWildCards #-}

module Build.NPM(rebuildNPM)
 where

import           Control.Monad(forM_, void, when)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource)
import           Data.Bifunctor(bimap)
import           Data.Bits((.|.))
import           Data.Conduit(sourceToList)
import qualified Data.Text as T
import           Data.Time.Clock(UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import           Database.Esqueleto
import           System.FilePath((</>), joinPath, makeRelative, splitDirectories)
import           System.Posix.Files(directoryMode, symbolicLinkMode)

import BDCS.Builds(insertBuild, insertBuildKeyValue)
import BDCS.DB
import BDCS.Files(associateFilesWithBuild, insertFile, sourceIdToFiles)
import BDCS.KeyType
import BDCS.NPM.SemVer(SemVer, SemVerRangeSet, parseSemVer, parseSemVerRangeSet, satisfies, toText)

rebuildNPM :: (MonadBaseControl IO m, MonadIO m, MonadError String m, MonadResource m) => Key Sources -> SqlPersistT m [Key Builds]
rebuildNPM sourceId = do
    -- get the name and version for this source
    (name, version) <- getNameVer

    -- figure out what sources satisfy the dependencies for this package
    -- Each list element represents one of the dependencies, within those each
    -- element is a source ID that satisfies the dependencies. sequence the whole thing
    -- to get all of the possible combinations that satisfy all dependencies.
    dependencies <- sequence <$> getDeps

    -- get the list of files for this source
    sourceFiles <- sourceToList $ sourceIdToFiles sourceId

    -- For each dependency list, create a new build
    mapM (relink sourceFiles (name, version)) dependencies
 where
    copyFile :: Files -> FilePath -> Files
    copyFile f@Files{..} newPath = let
        basePath = makeRelative "/package" $ T.unpack filesPath
     in
        f {filesPath = T.pack $ newPath </> basePath}

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

    relink :: (MonadBaseControl IO m, MonadIO m) => [Files] -> (T.Text, T.Text) -> [(T.Text, SemVer)] -> SqlPersistT m (Key Builds)
    relink sourceFiles (name, ver) depList = do
        buildTime <- liftIO getCurrentTime

        -- Create a directory for this module in /usr/lib/node_modules
        -- NB: In order to allow multiple versions of an npm module to be included in the same export,
        -- the /usr/lib/node_modules name is <module-name>@<module-version> instead of just <module-name>,
        -- and none of the bin or man symlinks are installed to /usr/bin and /usr/share/man. It's up to the
        -- export to determine which modules need to be accessible system-wide and to create the bin and man
        -- symlinks and the /usr/lib/node_modules/<module-name> directory.
        let module_dir = "/" </> "usr" </> "lib" </> "node_modules" </> T.unpack (T.concat [name, "@", ver])

        -- Create the /usr/lib/node_modules/<package> directory, and a node_modules directory under that
        moduleDirIds <- mkdirs buildTime $ module_dir </> "node_modules"

        -- Copy everything from the Source into the module directory
        packageIds <- mapM (insert . (`copyFile` module_dir)) sourceFiles

        -- For each of the dependencies, create a symlink from the /usr/lib/node_modules/<name>@<version> directory
        -- to this module's node_modules directory.
        deplinkIds <- mapM (createDepLink module_dir buildTime) depList

        -- Create a build and add the files to it
        createBuild $ moduleDirIds ++ packageIds ++ deplinkIds
     where
        createDepLink :: MonadIO m => FilePath -> UTCTime -> (T.Text, SemVer) -> SqlPersistT m (Key Files)
        createDepLink module_dir buildTime (depname, depver) = let
            verstr = toText depver
            source = T.pack $ joinPath ["/", "usr", "lib", "node_modules", T.unpack (T.concat [depname, "@", verstr])]
            dest   = T.pack $ joinPath [module_dir, "node_modules", T.unpack depname]
            link   = Files dest "root" "root" (floor $ utcTimeToPOSIXSeconds buildTime) Nothing (fromIntegral $ symbolicLinkMode .|. 0o0644) 0 (Just source)
         in
            insertFile link

        mkdirs :: MonadIO m => UTCTime -> FilePath -> SqlPersistT m [Key Files]
        mkdirs buildTime path = mapM mkdir $ scanl1 (</>) $ splitDirectories path
         where
            mkdir :: MonadIO m => FilePath -> SqlPersistT m (Key Files)
            mkdir subPath = insertFile $ Files (T.pack subPath) "root" "root" (floor $ utcTimeToPOSIXSeconds buildTime) Nothing (fromIntegral $ directoryMode .|. 0o0755) 0 Nothing

        createBuild :: MonadIO m => [Key Files] -> SqlPersistT m (Key Builds)
        createBuild fids = do
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
            void $ associateFilesWithBuild fids buildId

            -- Record the exact-version dependencies used for this build
            forM_ depList $ \(n, v) -> insertBuildKeyValue (TextKey "dependency") n (Just $ toText v) buildId

            return buildId
