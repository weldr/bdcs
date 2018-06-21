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
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Conditional(unlessM)
import Control.Exception(catch)
import Control.Monad(forM_)
import Control.Monad.Except(MonadError, runExceptT, throwError)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Reader(ReaderT, runReaderT)
import Data.ContentStore(CsError(..), mkContentStore)
import Data.List(isSuffixOf)
import Network.URI(URI(..), parseURI)
import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import           BDCS.Exceptions(DBException)
import qualified BDCS.Import.Comps as Comps
import qualified BDCS.Import.NPM as NPM
import qualified BDCS.Import.RPM as RPM
import qualified BDCS.Import.Repodata as Repodata
import           BDCS.Import.URI(isCompsFile, isPrimaryXMLFile)
import           BDCS.Import.State(ImportState(..))
import           BDCS.Utils.Either(whenLeft)
import           BDCS.Version

import Utils.GetOpt(commandLineArgs)

processThing :: String -> ReaderT ImportState IO ()
processThing url = case parseURI url of
    Just uri@URI{..} -> if | isPrimaryXMLFile uri           -> Repodata.loadFromURI uri
                           | isCompsFile uri                -> Comps.loadFromURI uri
                           | ".rpm" `isSuffixOf` uriPath    -> RPM.loadFromURI uri
                           | uriScheme == "npm:"            -> NPM.loadFromURI uri
                           | otherwise                      -> Repodata.loadRepoFromURI uri
    _ -> liftIO usage

runCommand :: (MonadError CsError m, MonadIO m) => FilePath -> FilePath -> [String] -> m ()
runCommand db repo things = do
    cs <- mkContentStore repo
    let st = ImportState { stDB=db, stRepo=cs }

    unlessM (liftIO $ doesFileExist db) $
         throwError $ CsError "Database must already exist - create with sqlite3 schema.sql"

    forM_ things $ \path ->
        liftIO $ runReaderT (processThing path) st

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

main :: IO ()
main = commandLineArgs <$> getArgs >>= \case
    Nothing               -> usage
    Just (db, repo, args) -> do
        result <- catch (runExceptT $ runCommand db repo args)
                        (\(e :: DBException) -> return $ Left $ CsError $ show e)
        whenLeft result (\e -> putStrLn $ "error: " ++ show e)
