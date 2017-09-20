{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Conditional(unlessM)
import           Control.Exception(Handler(..), catches, throw)
import           Control.Monad(forM_)
import           Control.Monad.Except(MonadError, runExceptT)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Aeson((.=), ToJSON, object, toJSON)
import           Data.Aeson.Encode.Pretty(encodePretty)
import           Data.ByteString.Lazy(toStrict)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Data.Maybe(catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
import           Data.Time.Clock.POSIX(getCurrentTime, posixSecondsToUTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           GI.OSTree(IsRepo)
import           System.Console.GetOpt
import           System.Directory(doesDirectoryExist, doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Printf(printf)
import           Text.Regex.PCRE((=~))

import           BDCS.DB(Files(..), KeyVal(..), checkAndRunSqlite)
import qualified BDCS.CS as CS
import           BDCS.Files(filesC, getKeyValuesForFile)
import           BDCS.KeyType(KeyType(..))
import           BDCS.KeyValue(keyValueListToJSON)
import           BDCS.Label.Types(Label, labelDescriptions)
import           BDCS.Version
import           Utils.Either(whenLeft)
import           Utils.Mode(modeAsText)

import Utils.Exceptions(InspectErrors(..))
import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)
import Utils.KeyVal(formatKeyValList)

data LsOptions = LsOptions { lsJSONOutput :: Bool,
                             lsKeyVal :: Bool,
                             lsLabelMatches :: Maybe Label,
                             lsMatches :: String,
                             lsVerbose :: Bool }

instance OptClass LsOptions

defaultLsOptions :: LsOptions
defaultLsOptions = LsOptions { lsJSONOutput = False,
                               lsKeyVal = False,
                               lsLabelMatches = Nothing,
                               lsMatches = ".*",
                               lsVerbose = False }

data LsRow = LsRow { rowFiles :: Files,
                     rowKeyVals :: Maybe [KeyVal],
                     rowMetadata :: Maybe CS.Object }

instance ToJSON LsRow where
    toJSON r = let namePair = T.pack "path" .= toJSON (filesPath $ rowFiles r)
                   keyvals  = maybe [] keyValueListToJSON (rowKeyVals r)
                   optional = catMaybes [ -- You may be tempted to rewrite the beginnings of these as "fileTypeString (rowMetadata r)",
                                          -- but don't.  We first want to check that there is any rowMetadata and if not, return Nothing.
                                          -- Doing it the other way passes Nothing to fileTypeString, which is set up to return something
                                          -- when given Nothing.
                                          rowMetadata r >>= fileTypeString . Just >>= \ty     -> Just $ T.pack "fileType" .= toJSON ty,
                                          rowMetadata r >>= csMetadata . Just     >>= \md     -> Just $ T.pack "mode" .= toJSON (CS.mode md),
                                          rowMetadata r >>= csMetadata . Just     >>= \md     -> Just $ T.pack "size" .= toJSON (CS.size md),
                                          rowMetadata r >>= symlinkTarget . Just  >>= \target -> Just $ T.pack "symlinkTarget" .= toJSON target,

                                          -- rowMetadata is only here as a test, which is why the result is thrown away.
                                          -- If rowMetadata is not Nothing, it means we were told to be verbose.  We only
                                          -- want to display user and group in verbose mode.
                                          rowMetadata r >>= \_ -> Just $ T.pack "user" .= toJSON (filesFile_user $ rowFiles r),
                                          rowMetadata r >>= \_ -> Just $ T.pack "group" .= toJSON (filesFile_group $ rowFiles r),

                                          -- Don't do any special formatting of the mtime - leave that up to the consumer.
                                          rowMetadata r >>= \_ -> Just $ T.pack "mtime" .= toJSON (filesMtime $ rowFiles r)
                               ]
               in
                   object $ [namePair] ++ keyvals ++ optional

initRow :: Files -> LsRow
initRow f = LsRow { rowFiles=f,
                    rowMetadata=Nothing,
                    rowKeyVals=Nothing }

fileType :: Maybe CS.Object -> Maybe Char
fileType (Just (CS.DirMeta _)) = Just 'd'
fileType (Just (CS.FileObject CS.FileContents{symlink=Just _})) = Just 'l'
fileType _ = Nothing

fileTypeString :: Maybe CS.Object -> Maybe String
fileTypeString (Just (CS.DirMeta _)) = Just "Directory"
fileTypeString (Just (CS.FileObject CS.FileContents{symlink=Just _})) = Just "Symlink"
fileTypeString _ = Just "File"

csMetadata :: Maybe CS.Object -> Maybe CS.Metadata
csMetadata (Just (CS.DirMeta metadata)) = Just metadata
csMetadata (Just (CS.FileObject CS.FileContents{metadata})) = Just metadata
csMetadata _ = Nothing

-- Figure out how to format the file's time.  If the time is in the current year, display
-- month, day, and hours/minutes.  If the time is in any other year, display that year
-- instead of hours and minutes.  This is not quite how ls does it - it appears to use
-- the threshold of if the file is more than a year old.  That's more time manipulation
-- than I am willing to do.
showTime :: Real t => String -> t -> String
showTime currentYear mtime = let
    utcMtime  = posixSecondsToUTCTime $ realToFrac mtime
    mtimeYear = formatTime defaultTimeLocale "%Y" utcMtime
    fmt       = "%b %e " ++ if currentYear == mtimeYear then "%R" else "%Y"
 in
    formatTime defaultTimeLocale fmt utcMtime

symlinkTarget :: Maybe CS.Object -> Maybe String
symlinkTarget (Just (CS.FileObject CS.FileContents{symlink=Just x})) = Just $ T.unpack x
symlinkTarget _ = Nothing

keyValToLabel :: KeyVal -> Maybe Label
keyValToLabel KeyVal {keyValKey_value=LabelKey x} = Just x
keyValToLabel _                                   = Nothing

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db repoPath args = do
    repo <- CS.open repoPath
    (opts, _) <- compilerOpts options defaultLsOptions args "ls"

    printer <- if | lsJSONOutput opts -> return $ liftedPutStrLn . jsonPrinter
                  | lsVerbose opts -> do currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
                                         return $ liftedPutStrLn . verbosePrinter currentYear
                  | otherwise -> return $ liftedPutStrLn . simplePrinter

    result <- runExceptT $ checkAndRunSqlite db $ runConduit $
              -- Grab all the Files, filtering out any whose path does not match what we want.
              filesC .| CL.filter (\f -> T.unpack (filesPath f) =~ lsMatches opts)
              -- Convert them into LsRow records containing only the Files record.
                     .| CL.map    initRow
              -- If we were asked for verbose output, add that to the LsRow.
                     .| CL.mapM   (\row -> if lsVerbose opts then do
                                               md <- getMetadata repo (rowFiles row)
                                               return $ row { rowMetadata=md }
                                           else return row)
              -- keyval output comes up in two different ways:  If we were
              -- given the --keyval flag, we want to add them to the LsRow,
              -- If we were given the --label flag, we want to grab the keyvals
              -- from the database and check for a match.  Note that both flags
              -- could be given at the same time.
                     .| CL.mapMaybeM (\row -> do kvs <- if lsKeyVal opts || isJust (lsLabelMatches opts)
                                                        then getKeyValuesForFile (filesPath $ rowFiles row)
                                                        else return []

                                                 let labels = mapMaybe keyValToLabel kvs

                                                 if | maybe False (`notElem` labels) (lsLabelMatches opts) -> return Nothing
                                                    | lsKeyVal opts -> return $ Just $ row { rowKeyVals=Just kvs }
                                                    | otherwise -> return $ Just row)
              -- Finally, pass it to the appropriate printer.
                     .| CL.mapM_  printer
    whenLeft result print
 where
    options :: [OptDescr (LsOptions -> LsOptions)]
    options = [
        Option [] ["json"]
               (NoArg (\opts -> opts { lsJSONOutput = True }))
               "format output as JSON",
        Option ['k'] ["keyval"]
               (NoArg (\opts -> opts { lsKeyVal = True }))
               "add key/val pairs to output",
        Option ['l'] []
               (NoArg (\opts -> opts { lsVerbose = True }))
               "use a long listing format",
        Option [] ["label"]
               (ReqArg (\d opts -> case reads d :: [(Label, String)] of
                                       [(lbl, _)] -> opts { lsLabelMatches = Just lbl }
                                       _          -> throw $ InvalidLabelError d) "LABEL")
               "return only results with the given LABEL",
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { lsMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

    getMetadata :: (IsRepo a, MonadIO m, MonadError String m) => a -> Files -> m (Maybe CS.Object)
    getMetadata repo Files{..} = case filesCs_object of
        Nothing    -> return Nothing
        Just cksum -> Just <$> CS.load repo cksum

    jsonPrinter :: LsRow -> T.Text
    jsonPrinter = decodeUtf8 . toStrict . encodePretty

    simplePrinter :: LsRow -> T.Text
    simplePrinter LsRow{..} = T.pack $
        printf "%s%s"
               (filesPath rowFiles)
               (maybe "" formatKeyValList rowKeyVals)

    verbosePrinter :: String -> LsRow -> T.Text
    verbosePrinter currentYear LsRow{..} = T.pack $
        printf "%c%s %8s %8s %10Ld %s %s%s%s"
               (fromMaybe '-' (fileType rowMetadata))
               (maybe "--ghost--" (T.unpack . modeAsText . CS.mode) (csMetadata rowMetadata))
               (T.unpack $ filesFile_user rowFiles) (T.unpack $ filesFile_group rowFiles)
               (maybe 0 CS.size (csMetadata rowMetadata))
               (showTime currentYear $ filesMtime rowFiles)
               (filesPath rowFiles) (maybe "" (" -> " ++) (symlinkTarget rowMetadata))
               (maybe "" formatKeyValList rowKeyVals)

usage :: IO ()
usage = do
    printVersion "inspect-ls"
    putStrLn "Usage: inspect-ls output.db repo [args ...]"
    putStrLn "  List files in the content store"
    putStrLn "- output.db is the path to a metadata database"
    putStrLn "- repo is the path to a content store repo"
    exitFailure

runMain :: IO ()
runMain = do
    argv <- getArgs
    case commandLineArgs argv of
        Nothing               -> usage
        Just (db, repo, args) -> do
            unlessM (doesFileExist db) $
                throw MissingDBError

            unlessM (doesDirectoryExist repo) $
                throw MissingCSError

            runCommand (T.pack db) repo args

main :: IO ()
main =
    -- Add handlers for other exception types (IOException, whatever) here.
    runMain `catches` [Handler (\(e :: InspectErrors) -> handleInspectErrors e)]
 where
    -- And then add handlers for the various kinds of InspectErrors here.
    handleInspectErrors :: InspectErrors -> IO ()
    handleInspectErrors (InvalidLabelError lbl) = do
        putStrLn $ lbl ++ " is not a recognized file label\n"
        putStrLn "Recognized labels:\n"
        forM_ labelDescriptions $ \(l, d) ->
            putStrLn $ "      " ++ l ++ " - " ++ d
        exitFailure

    handleInspectErrors MissingCSError = putStrLn "Content store does not exist\n" >> usage

    handleInspectErrors MissingDBError = putStrLn "Metadata database does not exist\n" >> usage
