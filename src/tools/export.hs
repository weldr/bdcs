-- Copyright (C) 2017-2018 Red Hat, Inc.
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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Conditional(ifM)
import           Control.Monad.Except(runExceptT)
import           Data.Maybe(fromJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Console.GetOpt
import           System.Directory(doesFileExist, removePathForcibly)
import           System.Environment(getArgs)
import           System.Exit(exitFailure, exitSuccess)

import BDCS.DB(checkAndRunSqlite)
import BDCS.Export(export)
import BDCS.Export.Types(exportTypeText, exportTypeFromText, supportedExportTypes)
import BDCS.Utils.Monad(concatMapM)
import BDCS.Version

import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)

data ExportOpts = ExportOpts { optDest :: FilePath,
                               optExportType :: T.Text }

instance OptClass ExportOpts

defaultExportOpts :: ExportOpts
defaultExportOpts = ExportOpts { optDest = "",
                                 optExportType = "tar" }

options :: [OptDescr (ExportOpts -> ExportOpts)]
options = [
    Option ['d'] ["dest"]
           (ReqArg (\d opts -> opts { optDest = d }) "DEST")
           "destination",
    Option ['t'] ["type"]
           (ReqArg (\t opts -> opts { optExportType = T.pack t }) "TYPE")
           "export type"
 ]

-- | Check a list of strings to see if any of them are files
-- If it is, read it and insert its contents in its place
expandFileThings :: [String] -> IO [String]
expandFileThings = concatMapM isThingFile
  where
    isThingFile :: String ->  IO [String]
    isThingFile thing = ifM (doesFileExist thing)
                            (lines <$> readFile thing)
                            (return [thing])

validExportTypes :: T.Text
validExportTypes = T.intercalate ", " (map exportTypeText supportedExportTypes)

usage :: IO ()
usage = do
    printVersion "export"
    putStrLn   "Usage: export metadata.db repo -t [export type] -d [dest] thing [thing ...]"
    putStrLn   "metadata.db - The path to an existing metadata repo"
    putStrLn   "repo        - The path to an existing content store"
    putStrLn $ "export type - One of the supported export types: " ++ T.unpack validExportTypes
    putStrLn   "dest        - The name of the export artifact to be created"
    putStrLn   "thing       -"
    putStrLn   "\t* The NEVRA of an RPM, e.g. filesystem-3.2-21.el7.x86_64"
    putStrLn   "\t* A path to a file containing NEVRA of RPMs, 1 per line."
    -- TODO group id?
    exitFailure

main :: IO ()
main = commandLineArgs <$> getArgs >>= \case
    Nothing               -> usage
    Just (db, repo, args) -> do
        (ExportOpts{..}, things) <- compilerOpts options defaultExportOpts args "export"
        things'                  <- map T.pack <$> expandFileThings things

        if | isNothing (exportTypeFromText optExportType) -> do
                 TIO.putStrLn $ "Invalid export type.  Valid types are: " `T.append` validExportTypes
                 exitFailure
           | null optDest -> do
                 putStrLn "No export destination given."
                 exitFailure
           | null things' -> do
                 putStrLn "Nothing to export."
                 exitFailure
           | otherwise -> do
                 let ty = fromJust $ exportTypeFromText optExportType
                 runExceptT (checkAndRunSqlite (T.pack db) $ export repo optDest ty things') >>= \case
                     Left err -> removePathForcibly optDest >> print err >> exitFailure
                     Right _  -> exitSuccess
