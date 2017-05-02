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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Conditional(unlessM)
import Control.Exception(catch)
import Control.Monad(void, when)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ReaderT, runReaderT)
import Data.List(isInfixOf, isSuffixOf)
import Network.URI(URI(..), parseURI, pathSegments)
import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hPutStrLn, stderr)

import qualified BDCS.CS as CS
import           BDCS.Exceptions(DBException)
import qualified Import.Comps as Comps
import qualified Import.RPM as RPM
import qualified Import.Repodata as Repodata
import           Import.State(ImportState(..))

processThing :: String -> ReaderT ImportState IO ()
processThing url = case parseURI url of
    Just uri@URI{..} -> if | isPrimaryXMLFile uri           -> Repodata.loadFromURI uri
                           | isCompsFile uri                -> Comps.loadFromURI uri
                           | ".rpm" `isSuffixOf` uriPath    -> RPM.loadFromURI uri
                           | otherwise                      -> Repodata.loadRepoFromURI uri
    _ -> liftIO usage
 where
    isPrimaryXMLFile :: URI -> Bool
    isPrimaryXMLFile uri = "primary.xml" `isInfixOf` last (pathSegments uri)

    isCompsFile :: URI -> Bool
    isCompsFile uri = let path = last (pathSegments uri) in
        "-comps" `isInfixOf` path && (".xml" `isSuffixOf` path || ".xml.gz" `isSuffixOf` path)

usage :: IO ()
usage = do
    putStrLn "Usage: test output.db repo thing [thing ...]"
    putStrLn "- repo is the path to an already existing content store repo or "
    putStrLn "  the path to a repo to be created"
    putStrLn "- thing can be:"
    putStrLn "\t* An HTTP, HTTPS, or file: URL to an RPM"
    putStrLn "\t* A URL to a yum repo primary.xml.gz file"
    putStrLn "\t* A URL to a yum repo comps.xml.gz file"
    exitFailure

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    -- Read the list of objects to import from the command line arguments
    argv <- getArgs

    when (length argv < 3) usage

    let db     = argv !! 0
    repo      <- CS.open (argv !! 1)
    let things = drop 2 argv

    unlessM (doesFileExist db) $ do
        putStrLn "Database must already exist - create with sqlite3 schema.sql"
        exitFailure

    let st = ImportState { stDB=db,
                           stRepo=repo }

    mapM_ (processOne st) things
 where
    processOne st path = catch (runReaderT (processThing path) st)
                               (\(e :: DBException) -> void $ hPutStrLn stderr ("*** Error importing " ++ path ++ ": " ++ show e))
