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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Conditional(unlessM)
import           Control.Monad(when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock.POSIX(getCurrentTime, posixSecondsToUTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           Database.Persist.Sqlite(runSqlite)
import           GI.OSTree(IsRepo)
import           System.Console.GetOpt
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Printf(printf)
import           Text.Regex.PCRE((=~))

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(filesC)
import           BDCS.Groups(groupsC, groupIdToNevra)
import           BDCS.Version
import           Utils.Either(whenLeft)
import           Utils.Mode(modeAsText)

-- These warnings are coming from options records that only have one field.
-- As options are added, these warnings will go away.  Until then, ignore
-- them.
{-# ANN module "HLint: ignore Use newtype instead of data" #-}

class OptClass a

data GroupsOptions = GroupsOptions { grpMatches :: String }

instance OptClass GroupsOptions

defaultGroupsOptions :: GroupsOptions
defaultGroupsOptions = GroupsOptions { grpMatches = ".*" }

data LsOptions = LsOptions { lsMatches :: String,
                             lsVerbose :: Bool }

instance OptClass LsOptions

defaultLsOptions :: LsOptions
defaultLsOptions = LsOptions { lsMatches = ".*",
                               lsVerbose = False }

data NevrasOptions = NevrasOptions { nevraMatches :: String }

instance OptClass NevrasOptions

defaultNevrasOptions :: NevrasOptions
defaultNevrasOptions = NevrasOptions { nevraMatches = ".*" }

compilerOpts :: OptClass a => [OptDescr (a -> a)] -> a -> [String] -> String -> IO (a, [String])
compilerOpts options defaults argv cmdName =
    case getOpt Permute options argv of
        (o, n, [])   -> return (foldl (flip id) defaults o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where
     header = "Usage: " ++ cmdName ++ " [OPTIONS]"

liftedPutStrLn :: MonadIO m => T.Text -> m ()
liftedPutStrLn = liftIO . TIO.putStrLn

runGroupsCommand :: T.Text -> [String] -> IO ()
runGroupsCommand db args = do
    (opts, _) <- compilerOpts options defaultGroupsOptions args "groups"
    runSqlite db $ runConduit $
        groupsC .| CL.map snd
                .| CL.filter (\g -> T.unpack g =~ grpMatches opts)
                .| CL.mapM_ liftedPutStrLn
 where
    options :: [OptDescr (GroupsOptions -> GroupsOptions)]
    options = [
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { grpMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

runLsCommand :: IsRepo a => T.Text -> a -> [String] -> IO ()
runLsCommand db repo args = do
    (opts, _) <- compilerOpts options defaultLsOptions args "ls"
    if lsVerbose opts then do
        currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
        result <- runExceptT $ runSqlite db $ runConduit $
                  filesC .| CL.filter (\f -> T.unpack (filesPath f) =~ lsMatches opts)
                         .| CL.mapM getMetadata
                         .| CL.mapM_ (liftedPutStrLn . verbosePrinter currentYear)
        whenLeft result print
    else
        runSqlite db $ runConduit $
        filesC .| CL.filter (\f -> T.unpack (filesPath f) =~ lsMatches opts)
               .| CL.mapM_ (liftedPutStrLn . filesPath)
 where
    options :: [OptDescr (LsOptions -> LsOptions)]
    options = [
        Option ['l'] []
               (NoArg (\opts -> opts { lsVerbose = True }))
               "use a long listing format",
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { lsMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

    getMetadata f@Files{..} = case filesCs_object of
        Nothing    -> return (f, Nothing)
        Just cksum -> CS.load repo cksum >>= \obj -> return (f, Just obj)

    verbosePrinter :: String -> (Files, Maybe CS.Object) -> T.Text
    verbosePrinter currentYear (Files{..}, obj) = T.pack $
        printf "%c%s %8s %8s %10Ld %s %s%s"
               ty
               (maybe "--ghost--" (T.unpack . modeAsText . CS.mode) md)
               (T.unpack filesFile_user) (T.unpack filesFile_group)
               (maybe 0 CS.size md)
               (showTime filesMtime)
               filesPath target
     where
        md = case obj of
            Just (CS.DirMeta metadata) -> Just metadata
            Just (CS.FileObject CS.FileContents{metadata}) -> Just metadata
            Nothing -> Nothing

        ty = case obj of
            Just (CS.DirMeta _) -> 'd'
            Just (CS.FileObject CS.FileContents{symlink=Just _}) -> 'l'
            _ -> '-'

        target = case obj of
            Just (CS.FileObject CS.FileContents{symlink=Just x}) -> " -> " ++ T.unpack x
            _ -> ""

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
runNevrasCommand db args = do
    (opts, _) <- compilerOpts options defaultNevrasOptions args "nevras"
    runSqlite db $ runConduit $
        groupsC .| CL.map fst
                .| CL.mapMaybeM groupIdToNevra
                .| CL.filter (\g -> T.unpack g =~ nevraMatches opts)
                .| CL.mapM_ liftedPutStrLn
 where
    options :: [OptDescr (NevrasOptions -> NevrasOptions)]
    options = [
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { nevraMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

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
