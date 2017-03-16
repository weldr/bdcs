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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Conditional(unlessM)
import           Control.Exception(catch)
import           Control.Monad(void, when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(MonadResource)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit((.|), ConduitM, Consumer, Producer, awaitForever, runConduitRes, yield)
import           Data.Conduit.Binary(sourceFile)
import           Data.Conduit.Zlib(ungzip)
import           Data.List(isInfixOf, isSuffixOf)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import qualified Data.Text as T
import           Network.HTTP.Conduit(path)
import           Network.HTTP.Simple(Request, getResponseBody, httpSource, parseRequest)
import           Network.URI(URI(..), parseURI)
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), takeDirectory)
import           System.IO(hPutStrLn, stderr)
import           Text.XML(Document, sinkDoc)
import           Text.XML.Cursor
import           Text.XML.Stream.Parse(def)

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

-- TODO: Can this be brought into the conduit somehow?
extractLocations :: Document -> [String]
extractLocations doc = let
    cursor = fromDocument doc
 in
    -- Find all <location href=""> elements and return the href's value.  laxElement
    -- means we ignore case and ignore namespacing.  Otherwise we need to take into
    -- account the namespace given in the primary.xml.
    map T.unpack $
        cursor $// laxElement "location"
               >=> hasAttribute "href"
               >=> attribute "href"

--
-- WORKING WITH RPMS
--

-- Load a parsed RPM into the database.
loadRPM :: FilePath -> RPM -> IO ()
loadRPM db RPM{..} = runSqlite (T.pack db) $ unlessM (buildImported sigs) $ do
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

-- A conduit consumer that takes in RPM data and uses loadRPM to put them in the database.
consumeRPM :: MonadIO m => FilePath -> Consumer RPM m ()
consumeRPM db = awaitForever (liftIO . loadRPM db)

-- Put data from a file into a conduit.
getFromFile :: MonadResource m => FilePath -> Producer m BS.ByteString
getFromFile = sourceFile

-- Put data from a URL into a conduit.
getFromURL :: MonadResource m => Request -> Producer m BS.ByteString
getFromURL request = httpSource request getResponseBody

-- If a conduit is compressed, pass it through ungzip to uncompress it.  Otherwise, pass it
-- through without doing anything.  We can only tell if a conduit is compressed by also being
-- given the path to the thing being processed.
maybeUngzip :: MonadResource m => FilePath -> ConduitM BS.ByteString BS.ByteString m ()
maybeUngzip path | ".gz" `isSuffixOf` path  = ungzip
                 | otherwise                = awaitForever yield

processFromFile :: FilePath -> String -> IO ()
processFromFile db path = do
    void $ runExceptT $ runConduitRes (pipeline path)
    putStrLn $ "Imported " ++ path
 where
    pipeline f = getFromFile f .| parseRPMC .| consumeRPM db

processFromURL :: FilePath -> Request -> IO ()
processFromURL db request = do
    void $ runExceptT $ runConduitRes (pipeline request)
    C8.putStrLn $ BS.concat ["Imported ", path request]
 where
    pipeline r = getFromURL r .| parseRPMC .| consumeRPM db

processFromLocalRepodata :: FilePath -> String -> IO ()
processFromLocalRepodata db metadataPath = do
    locations <- map (takeDirectory metadataPath </>) <$> extractLocations <$> runConduitRes (readMetadataPipeline metadataPath)
    mapM_ (processFromFile db) locations
 where
    readMetadataPipeline path = getFromFile path .| maybeUngzip path .| sinkDoc def

processFromRepodata :: FilePath -> Request -> IO ()
processFromRepodata db metadataRequest = do
    let (basePath, _) = BS.breakSubstring "repodata/" (path metadataRequest)
    locations <- map (\p -> metadataRequest { path=BS.concat [basePath, C8.pack p] }) <$> extractLocations <$> runConduitRes (readMetadataPipeline metadataRequest)
    mapM_ (processFromURL db) locations
 where
    readMetadataPipeline request = getFromURL request .| maybeUngzip (C8.unpack $ path request) .| sinkDoc def

--
-- MAIN
--

buildImported :: MonadIO m => [Tag] ->  SqlPersistT m Bool
buildImported sigs =
    case findStringTag "SHA1Header" sigs of
        Just sha -> do ndx <- select $ from $ \signatures -> do
                              where_ (signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                      signatures ^. BuildSignaturesSignature_data ==. val (C8.pack sha))
                              return (signatures ^. BuildSignaturesId)
                       return $ not $ null ndx
        Nothing  -> return False

processThing :: FilePath -> String -> IO ()
processThing db url = do
    let parsed = parseURI url
    case parsed of
        Just URI{..} -> if | uriScheme == "file:" && "primary.xml" `isInfixOf` uriPath -> processFromLocalRepodata db uriPath
                           | uriScheme == "file:" && ".rpm" `isSuffixOf` uriPath       -> processFromFile db uriPath
                           | "primary.xml" `isInfixOf` uriPath                         -> parseRequest url >>= processFromRepodata db
                           | ".rpm" `isSuffixOf` uriPath                               -> parseRequest url >>= processFromURL db
                           | otherwise                                                 -> usage
        _ -> parseRequest url >>= processFromURL db

usage :: IO ()
usage = do
    putStrLn "Usage: test output.db thing [thing ...]"
    putStrLn "thing can be:"
    putStrLn "\t* An HTTP, HTTPS, or file: URL to an RPM"
    putStrLn "\t* A URL to a yum repo primary.xml.gz file"
    exitFailure

main :: IO ()
main = do
    -- Read the list of objects to import from the command line arguments
    argv <- getArgs

    when (length argv < 2) usage

    let db     = head argv
    let things = tail argv

    unlessM (doesFileExist db) $ do
        putStrLn "Database must already exist - create with sqlite3 schema.sql"
        exitFailure

    mapM_ (processOne db) things
 where
    processOne db path = catch (processThing db path)
                               (\(e :: DBException) -> void $ hPutStrLn stderr ("*** Error importing " ++ path ++ ": " ++ show e))
