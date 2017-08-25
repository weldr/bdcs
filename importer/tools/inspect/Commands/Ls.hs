{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Conditional(unlessM)
import           Control.Monad.Except(MonadError, runExceptT)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Data.List(intercalate)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(getCurrentTime, posixSecondsToUTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           Database.Persist.Sqlite(runSqlite)
import           GI.OSTree(IsRepo)
import           System.Console.GetOpt
import           System.Directory(doesDirectoryExist, doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Printf(printf)
import           Text.Regex.PCRE((=~))

import           BDCS.DB(Files(..), KeyVal(..))
import qualified BDCS.CS as CS
import           BDCS.Files(filesC, getKeyValuesForFile)
import           BDCS.KeyValue(formatKeyValue)
import           BDCS.Version
import           Utils.Either(whenLeft)
import           Utils.Mode(modeAsText)

import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)

data LsOptions = LsOptions { lsKeyVal :: Bool,
                             lsMatches :: String,
                             lsVerbose :: Bool }

instance OptClass LsOptions

defaultLsOptions :: LsOptions
defaultLsOptions = LsOptions { lsKeyVal = False,
                               lsMatches = ".*",
                               lsVerbose = False }

data LsRow = LsRow { rowFiles :: Files,
                     rowKeyVals :: Maybe [KeyVal],
                     rowMetadata :: Maybe CS.Object }

initRow :: Files -> LsRow
initRow f = LsRow { rowFiles=f,
                    rowMetadata=Nothing,
                    rowKeyVals=Nothing }

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db repoPath args = do
    repo <- CS.open repoPath
    (opts, _) <- compilerOpts options defaultLsOptions args "ls"
    printer <- if lsVerbose opts then do
        currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
        return $ liftedPutStrLn . verbosePrinter currentYear
     else
        return $ liftedPutStrLn . simplePrinter

    result <- runExceptT $ runSqlite db $ runConduit $
              -- Grab all the Files, filtering out any whose path does not match what we want.
              filesC .| CL.filter (\f -> T.unpack (filesPath f) =~ lsMatches opts)
              -- Convert them into LsRow records containing only the Files record.
                     .| CL.map    initRow
              -- If we were asked for verbose output, add that to the LsRow.
                     .| CL.mapM   (\row -> if lsVerbose opts then do
                                               md <- getMetadata repo (rowFiles row)
                                               return $ row { rowMetadata=md }
                                           else return row)
              -- If we were asked for keyval output, add that to the LsRow.
                     .| CL.mapM   (\row -> if lsKeyVal opts then do
                                               kvs <- getKeyValuesForFile (filesPath $ rowFiles row)
                                               return $ row { rowKeyVals=Just kvs }
                                           else return row)
              -- Finally, pass it to the appropriate printer.
                     .| CL.mapM_  printer
    whenLeft result print
 where
    options :: [OptDescr (LsOptions -> LsOptions)]
    options = [
        Option ['k'] ["keyval"]
               (NoArg (\opts -> opts { lsKeyVal = True }))
               "add key/val pairs to output",
        Option ['l'] []
               (NoArg (\opts -> opts { lsVerbose = True }))
               "use a long listing format",
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { lsMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

    getMetadata :: (IsRepo a, MonadIO m, MonadError String m) => a -> Files -> m (Maybe CS.Object)
    getMetadata repo Files{..} = case filesCs_object of
        Nothing    -> return Nothing
        Just cksum -> Just <$> CS.load repo cksum

    simplePrinter :: LsRow -> T.Text
    simplePrinter LsRow{..} = T.pack $
        printf "%s%s"
               (filesPath rowFiles)
               keyvals
     where
        keyvals = case rowKeyVals of
            Just lst -> printf " [%s]" (intercalate ", " (map (T.unpack . formatKeyValue) lst))
            _ -> ""

    verbosePrinter :: String -> LsRow -> T.Text
    verbosePrinter currentYear LsRow{..} = T.pack $
        printf "%c%s %8s %8s %10Ld %s %s%s%s"
               ty
               (maybe "--ghost--" (T.unpack . modeAsText . CS.mode) md)
               (T.unpack $ filesFile_user rowFiles) (T.unpack $ filesFile_group rowFiles)
               (maybe 0 CS.size md)
               (showTime $ filesMtime rowFiles)
               (filesPath rowFiles) target
               keyvals
     where
        md = case rowMetadata of
            Just (CS.DirMeta metadata) -> Just metadata
            Just (CS.FileObject CS.FileContents{metadata}) -> Just metadata
            Nothing -> Nothing

        ty = case rowMetadata of
            Just (CS.DirMeta _) -> 'd'
            Just (CS.FileObject CS.FileContents{symlink=Just _}) -> 'l'
            _ -> '-'

        target = case rowMetadata of
            Just (CS.FileObject CS.FileContents{symlink=Just x}) -> " -> " ++ T.unpack x
            _ -> ""

        keyvals = case rowKeyVals of
            Just lst -> printf " [%s]" (intercalate ", " (map (T.unpack . formatKeyValue) lst))
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

usage :: IO ()
usage = do
    printVersion "inspect-ls"
    putStrLn "Usage: inspect-ls output.db repo [args ...]"
    putStrLn "  List files in the content store"
    putStrLn "- output.db is the path to a metadata database"
    putStrLn "- repo is the path to a content store repo"
    exitFailure

main :: IO ()
main = do
    argv <- getArgs
    case commandLineArgs argv of
        Nothing               -> usage
        Just (db, repo, args) -> do
            unlessM (doesFileExist db) $ do
                putStrLn "database does not exist"
                exitFailure

            unlessM (doesDirectoryExist repo) $ do
                putStrLn "content store does not exist"
                exitFailure

            runCommand (T.pack db) repo args
