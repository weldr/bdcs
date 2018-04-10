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

{-# LANGUAGE LambdaCase #-}

import           Control.Conditional(cond, ifM)
import           Control.Monad.Except(runExceptT)
import           Data.List(isSuffixOf)
import qualified Data.Text as T
import           System.Directory(doesFileExist, removePathForcibly)
import           System.Environment(getArgs)
import           System.Exit(exitFailure, exitSuccess)

import BDCS.DB(checkAndRunSqlite)
import BDCS.Export(export)
import BDCS.Export.Types(ExportType(..))
import BDCS.Utils.Monad(concatMapM)
import BDCS.Version

import Utils.GetOpt(commandLineArgs)

-- | Check a list of strings to see if any of them are files
-- If it is, read it and insert its contents in its place
expandFileThings :: [String] -> IO [String]
expandFileThings = concatMapM isThingFile
  where
    isThingFile :: String ->  IO [String]
    isThingFile thing = ifM (doesFileExist thing)
                            (lines <$> readFile thing)
                            (return [thing])

usage :: IO ()
usage = do
    printVersion "export"
    putStrLn "Usage: export metadata.db repo dest thing [thing ...]"
    putStrLn "dest can be:"
    putStrLn "\t* A directory (which may or may not already exist)"
    putStrLn "\t* The name of a .tar file to be created"
    putStrLn "\t* The name of a .qcow2 image to be created"
    putStrLn "\t* A directory ending in .repo, which will create a new ostree repo"
    putStrLn "thing can be:"
    putStrLn "\t* The NEVRA of an RPM, e.g. filesystem-3.2-21.el7.x86_64"
    putStrLn "\t* A path to a file containing NEVRA of RPMs, 1 per line."
    -- TODO group id?
    exitFailure

main :: IO ()
main = commandLineArgs <$> getArgs >>= \case
    Just (db, repo, out_path:things) -> do things' <- map T.pack <$> expandFileThings things

                                           let ty = cond [(".tar" `isSuffixOf` out_path,   ExportTar),
                                                          (".qcow2" `isSuffixOf` out_path, ExportQcow2),
                                                          (".repo" `isSuffixOf` out_path,  ExportOstree),
                                                          (otherwise,                      ExportDirectory)]

                                           result  <- runExceptT $ checkAndRunSqlite (T.pack db) $ export repo out_path ty things'
                                           case result of
                                               Left err -> removePathForcibly out_path >> print err >> exitFailure
                                               Right _  -> exitSuccess
    _                                -> usage
