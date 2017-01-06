-- Copyright (C) 2016 Red Hat, Inc.
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Conduit(MonadResource, awaitForever, runResourceT, sourceFile)
import           Control.Conditional(notM, whenM)
import           Control.Exception(catch)
import           Control.Monad(void, when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8(pack)
import           Data.Conduit(($$), (=$=), Consumer, Producer)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import qualified Data.Text as T
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.IO(hPutStrLn, stderr)

import BDCS.Builds(associateBuildWithPackage, insertBuild)
import BDCS.DB
import BDCS.Exceptions
import BDCS.Files(associateFilesWithBuild, associateFilesWithPackage, insertFiles)
import BDCS.Groups(createGroup)
import BDCS.Packages(insertPackageName)
import BDCS.Projects(insertProject)
import BDCS.Signatures(insertBuildSignatures)
import BDCS.Sources(insertSource)
import RPM.Parse(parseRPMC)
import RPM.Tags
import RPM.Types

--
-- WORKING WITH RPMS
--

loadRPM :: FilePath -> RPM -> IO ()
loadRPM db RPM{..} = runSqlite (T.pack db) $ whenM (notM $ buildImported sigs) $ do
    projectId <- insertProject tags
    sourceId  <- insertSource tags projectId
    buildId   <- insertBuild tags sourceId
    void $ insertBuildSignatures sigs buildId
    filesIds  <- insertFiles tags
    pkgNameId <- insertPackageName tags

    void $ associateFilesWithBuild filesIds buildId
    void $ associateFilesWithPackage filesIds pkgNameId
    void $ associateBuildWithPackage buildId pkgNameId

    -- groups and reqs
    -- groupId <- createGroup filesIds tags
    void $ createGroup filesIds tags
 where
    -- FIXME:  Be less stupid.
    sigs = headerTags $ head rpmHeaders
    tags = headerTags $ rpmHeaders !! 1

processRPM :: FilePath -> FilePath -> IO ()
processRPM db path = void $ runExceptT $ runResourceT pipeline
 where
    pipeline = getRPM path =$= parseRPMC $$ consumer

    getRPM :: MonadResource m => FilePath -> Producer m BS.ByteString
    getRPM = sourceFile

    consumer :: MonadIO m => Consumer RPM m ()
    consumer = awaitForever (liftIO . loadRPM db)

--
-- MAIN
--

buildImported :: MonadIO m => [Tag] ->  SqlPersistT m Bool
buildImported sigs =
    case findStringTag "SHA1Header" sigs of
        Just sha -> do ndx <- select $ from $ \signatures -> do
                              where_ (signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                      signatures ^. BuildSignaturesSignature_data ==. val (pack sha))
                              return (signatures ^. BuildSignaturesId)
                       return $ not $ null ndx
        Nothing  -> return False

main :: IO ()
main = do
    -- Read the list of rpms to process from the command line arguments
    argv <- getArgs

    when (length argv < 2) $ do
        putStrLn "Usage: test output.db RPM [RPM ...]"
        exitFailure

    let db   = head argv
    let rpms = tail argv

    initDB db
    mapM_ (processOne db) rpms
 where
    processOne db path = catch (processRPM db path >> putStrLn ("Imported " ++ path))
                               (\(e :: DBException) -> void $ hPutStrLn stderr ("*** Error importing RPM " ++ path ++ ": " ++ show e))
