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
import Data.ContentStore(mkContentStore, runCsMonad)
import Data.List(isSuffixOf)
import Network.URI(URI(..), parseURI)
import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hPutStrLn, stderr)

import           BDCS.Exceptions(DBException)
import           BDCS.Version
import qualified Import.Comps as Comps
import qualified Import.NPM as NPM
import qualified Import.RPM as RPM
import qualified Import.Repodata as Repodata
import           Import.URI(isCompsFile, isPrimaryXMLFile)
import           Import.State(ImportState(..))

processThing :: String -> ReaderT ImportState IO ()
processThing url = case parseURI url of
    Just uri@URI{..} -> if | isPrimaryXMLFile uri           -> Repodata.loadFromURI uri
                           | isCompsFile uri                -> Comps.loadFromURI uri
                           | ".rpm" `isSuffixOf` uriPath    -> RPM.loadFromURI uri
                           | uriScheme == "npm:"            -> NPM.loadFromURI uri
                           | otherwise                      -> Repodata.loadRepoFromURI uri
    _ -> liftIO usage

usage :: IO ()
usage = do
    printVersion "import"
    putStrLn "Usage: import output.db repo thing [thing ...]"
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
    let things = drop 2 argv

    result <- runCsMonad $ mkContentStore (argv !! 1)
    repo   <- case result of
                   Left e  -> print e >> exitFailure
                   Right r -> return r

    unlessM (doesFileExist db) $ do
        putStrLn "Database must already exist - create with sqlite3 schema.sql"
        exitFailure

    let st = ImportState { stDB=db,
                           stRepo=repo }

    mapM_ (processOne st) things
 where
    processOne st path = catch (runReaderT (processThing path) st)
                               (\(e :: DBException) -> void $ hPutStrLn stderr ("*** Error importing " ++ path ++ ": " ++ show e))
