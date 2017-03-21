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
import Data.List(isInfixOf, isSuffixOf)
import Network.HTTP.Simple(parseRequest)
import Network.URI(URI(..), parseURI)
import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hPutStrLn, stderr)

import           BDCS.Exceptions(DBException)
import qualified Import.Comps as Comps
import qualified Import.RPM as RPM
import qualified Import.Repodata as Repodata

processThing :: FilePath -> String -> IO ()
processThing db url = do
    let parsed = parseURI url
    case parsed of
        Just URI{..} -> if | uriScheme == "file:" && isPrimaryXMLFile uriPath       -> Repodata.loadFromFile db uriPath
                           | uriScheme == "file:" && isCompsFile uriPath            -> Comps.loadFromFile db uriPath
                           | uriScheme == "file:" && ".rpm" `isSuffixOf` uriPath    -> RPM.loadFromFile db uriPath

                           | isPrimaryXMLFile uriPath                               -> parseRequest url >>= Repodata.loadFromURL db
                           | isCompsFile uriPath                                    -> parseRequest url >>= Comps.loadFromURL db
                           | ".rpm" `isSuffixOf` uriPath                            -> parseRequest url >>= RPM.loadFromURL db

                           | otherwise                                              -> usage
        _ -> parseRequest url >>= RPM.loadFromURL db
 where
    isPrimaryXMLFile path = "primary.xml" `isInfixOf` path

    isCompsFile path = "-comps" `isInfixOf` path && (".xml" `isSuffixOf` path || ".xml.gz" `isSuffixOf` path)

usage :: IO ()
usage = do
    putStrLn "Usage: test output.db thing [thing ...]"
    putStrLn "thing can be:"
    putStrLn "\t* An HTTP, HTTPS, or file: URL to an RPM"
    putStrLn "\t* A URL to a yum repo primary.xml.gz file"
    putStrLn "\t* A URL to a yum repo comps.xml.gz file"
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
