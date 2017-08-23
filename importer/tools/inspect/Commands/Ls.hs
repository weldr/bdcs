{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Commands.Ls(runCommand)
 where

import           Control.Monad.Except(MonadError, runExceptT)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(getCurrentTime, posixSecondsToUTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           Database.Persist.Sqlite(runSqlite)
import           GI.OSTree(IsRepo)
import           System.Console.GetOpt
import           Text.Printf(printf)
import           Text.Regex.PCRE((=~))

import           BDCS.DB(Files(..))
import qualified BDCS.CS as CS
import           BDCS.Files(filesC)
import           Utils.Either(whenLeft)
import           Utils.Mode(modeAsText)

import Utils.GetOpt(OptClass, compilerOpts)
import Utils.IO(liftedPutStrLn)

-- These warnings are coming from options records that only have one field.
-- As options are added, these warnings will go away.  Until then, ignore
-- them.
{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data LsOptions = LsOptions { lsMatches :: String,
                             lsVerbose :: Bool }

instance OptClass LsOptions

defaultLsOptions :: LsOptions
defaultLsOptions = LsOptions { lsMatches = ".*",
                               lsVerbose = False }

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db repoPath args = do
    repo <- CS.open repoPath
    (opts, _) <- compilerOpts options defaultLsOptions args "ls"
    printer <- if lsVerbose opts then do
        currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
        return $ liftedPutStrLn . verbosePrinter currentYear
     else
        return $ liftedPutStrLn . filesPath . fst

    result <- runExceptT $ runSqlite db $ runConduit $
              filesC .| CL.filter (\f -> T.unpack (filesPath f) =~ lsMatches opts)
                     .| CL.mapM   (\f -> if lsVerbose opts then getMetadata repo f else return (f, Nothing))
                     .| CL.mapM_  printer
    whenLeft result print
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

    getMetadata :: (IsRepo a, MonadIO m, MonadError String m) => a -> Files -> m (Files, Maybe CS.Object)
    getMetadata repo f@Files{..} = case filesCs_object of
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
