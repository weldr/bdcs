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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Conditional(unlessM)
import           Control.Monad(when)
import           Control.Monad.IO.Class(liftIO)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock.POSIX(getCurrentTime, posixSecondsToUTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           Database.Persist.Sqlite(runSqlite)
import           GI.OSTree(IsRepo)
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(filesC)
import           BDCS.Groups(groupsC, groupIdToNevra)
import           BDCS.Version

liftedPutStrLn = liftIO . TIO.putStrLn

runGroupsCommand :: T.Text -> [String] -> IO ()
runGroupsCommand db _ =
    runSqlite db $ runConduit $ groupsC .| CL.mapM_ (liftedPutStrLn . snd)

runLsCommand :: IsRepo a => T.Text -> a -> [String] -> IO ()
runLsCommand db repo args =
    if "-l" `elem` args
    then do
        currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
        runSqlite db $ runConduit $ filesC .| CL.mapM getMetadata
                                           .| CL.catMaybes
                                           .| CL.mapM_ (liftedPutStrLn . verbosePrinter currentYear)
    else
        runSqlite db $ runConduit $ filesC .| CL.mapM_ (liftedPutStrLn . filesPath)
 where
    getMetadata f@Files{..} = case filesCs_object of
        Nothing    -> return Nothing
        Just cksum -> CS.load repo cksum >>= \case
            CS.DirMeta obj    -> return $ Just (f, obj)
            CS.FileObject obj -> return $ Just (f, CS.metadata obj)

    verbosePrinter :: String -> (Files, CS.Metadata) -> T.Text
    verbosePrinter currentYear (Files{..}, metadata) =
        T.concat [filesFile_user, " ", filesFile_group, " ", T.pack $ showTime filesMtime, " ", filesPath]
     where
        -- Figure out how to format the file's time.  If the time is in the current year, display
        -- month, day, and hours/minutes.  If the time is in any other year, display that year
        -- instead of hours and minutes.  This is not quite how ls does it - it appears to use
        -- the threshold of if the file is more than a year old.  That's more time manipulation
        -- than I am willing to do.
        showTime :: Int -> String
        showTime mtime = let
            utcMtime  = posixSecondsToUTCTime $ realToFrac mtime
            mtimeYear = formatTime defaultTimeLocale "%Y" utcMtime
            fmt       = "%b %e " ++ if currentYear == mtimeYear then "%R" else "%Y"
         in
            formatTime defaultTimeLocale fmt utcMtime

runNevrasCommand :: T.Text -> [String] -> IO ()
runNevrasCommand db _ =
    runSqlite db $ runConduit $ groupsC .| CL.map fst
                                        .| CL.mapMaybeM groupIdToNevra
                                        .| CL.mapM_ liftedPutStrLn

usage :: IO ()
usage = do
    printVersion "inspect"
    putStrLn "Usage: inspect output.db repo subcommand [args ...]"
    putStrLn "- output.db is the path to a metadata database"
    putStrLn "- repo is the path to a content store repo"
    putStrLn "- subcommands:"
    putStrLn "      groups - List groups (packages, etc.)"
    putStrLn "      ls     - List files"
    putStrLn "      nevras - List NEVRAs of RPM packages"
    exitFailure

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 3) usage

    let db     = argv !! 0
    repo      <- CS.open (argv !! 1)
    let subcmd = argv !! 2
    let args   = drop 3 argv

    unlessM (doesFileExist db) $ do
        putStrLn "Database does not exist"
        exitFailure

    case subcmd of
        "groups"    -> runGroupsCommand (T.pack db) args
        "ls"        -> runLsCommand (T.pack db) repo args
        "nevras"    -> runNevrasCommand (T.pack db) args
        _           -> usage
