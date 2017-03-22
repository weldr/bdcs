-- Copyright (C) 2016-2017 Red Hat, Inc.
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Import.RPM(consume,
                  load,
                  loadFromFile,
                  loadFromURL)
 where

import           Control.Conditional(unlessM)
import           Control.Monad(void)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit((.|), Consumer, awaitForever, runConduitRes)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import qualified Data.Text as T
import           Network.HTTP.Conduit(path)
import           Network.HTTP.Simple(Request)

import BDCS.Builds(associateBuildWithPackage, insertBuild)
import BDCS.DB
import BDCS.Exceptions(DBException(..), throwIfNothing)
import BDCS.Files(associateFilesWithBuild, associateFilesWithPackage, insertFiles)
import BDCS.Packages(insertPackageName)
import BDCS.Projects(insertProject)
import BDCS.Signatures(insertBuildSignatures)
import BDCS.Sources(insertSource)
import BDCS.RPM.Builds(mkBuild)
import BDCS.RPM.Files(mkFiles)
import BDCS.RPM.Groups(createGroup)
import BDCS.RPM.Projects(mkProject)
import BDCS.RPM.Signatures(mkRSASignature, mkSHASignature)
import BDCS.RPM.Sources(mkSource)
import Import.Conduit(getFromFile, getFromURL)
import RPM.Parse(parseRPMC)
import RPM.Tags
import RPM.Types

-- A conduit consumer that takes in RPM data and uses loadRPM to put them in the database.
consume :: MonadIO m => FilePath -> Consumer RPM m ()
consume db = awaitForever (liftIO . load db)

-- Load a parsed RPM into the database.
load :: FilePath -> RPM -> IO ()
load db RPM{..} = runSqlite (T.pack db) $ unlessM (buildImported sigs) $ do
    projectId <- insertProject $ mkProject tags
    sourceId  <- insertSource $ mkSource tags projectId
    buildId   <- insertBuild $ mkBuild tags sourceId
    void $ insertBuildSignatures [mkRSASignature sigs buildId, mkSHASignature sigs buildId]
    filesIds  <- mkFiles tags >>= insertFiles
    pkgNameId <- insertPackageName $ findStringTag "Name" tags `throwIfNothing` DBException "No Name tag in RPM"

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

    buildImported :: MonadIO m => [Tag] ->  SqlPersistT m Bool
    buildImported sigs =
        case findStringTag "SHA1Header" sigs of
            Just sha -> do ndx <- select $ from $ \signatures -> do
                                  where_ $ signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                           signatures ^. BuildSignaturesSignature_data ==. val (C8.pack sha)
                                  return $ signatures ^. BuildSignaturesId
                           return $ not $ null ndx
            Nothing  -> return False

loadFromFile :: FilePath -> String -> IO ()
loadFromFile db path = do
    void $ runExceptT $ runConduitRes (pipeline path)
    putStrLn $ "Imported " ++ path
 where
    pipeline f = getFromFile f .| parseRPMC .| consume db

loadFromURL :: FilePath -> Request -> IO ()
loadFromURL db request = do
    void $ runExceptT $ runConduitRes (pipeline request)
    C8.putStrLn $ BS.concat ["Imported ", path request]
 where
    pipeline r = getFromURL r .| parseRPMC .| consume db
