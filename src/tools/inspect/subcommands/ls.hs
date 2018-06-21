{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Conditional(unlessM)
import           Control.Exception(Handler(..), catches, throwIO)
import           Control.Monad(forM_)
import           Control.Monad.Except(runExceptT, when)
import           Data.Aeson((.=), ToJSON, object, toJSON)
import           Data.Aeson.Encode.Pretty(encodePretty)
import           Data.ByteString.Lazy(toStrict)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Data.Maybe(catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
import           Data.Time.Clock.POSIX(getCurrentTime, posixSecondsToUTCTime)
import           Data.Time.Format(defaultTimeLocale, formatTime)
import           System.Console.GetOpt
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Printf(printf)
import           Text.Read(readMaybe)
import           Text.Regex.PCRE((=~))

import BDCS.DB(Files(..), KeyVal(..), checkAndRunSqlite)
import BDCS.Files(filesC, getKeyValuesForFile)
import BDCS.KeyType(KeyType(..))
import BDCS.KeyValue(keyValueListToJSON)
import BDCS.Label.Types(Label, labelDescriptions)
import BDCS.Utils.Either(whenLeft)
import BDCS.Utils.Mode(modeAsText)
import BDCS.Version

import Utils.Exceptions(InspectErrors(..))
import Utils.GetOpt(commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)
import Utils.KeyVal(formatKeyValList)

data LsOptions = LsOptions { lsJSONOutput :: Bool,
                             lsKeyVal :: Bool,
                             lsLabelMatches :: Maybe Label,
                             lsMatches :: String,
                             lsVerbose :: Bool }

defaultLsOptions :: LsOptions
defaultLsOptions = LsOptions { lsJSONOutput = False,
                               lsKeyVal = False,
                               lsLabelMatches = Nothing,
                               lsMatches = ".*",
                               lsVerbose = False }

data LsRow = LsRow { rowFiles :: Files,
                     rowKeyVals :: Maybe [KeyVal],
                     rowUseMetadata :: Bool }

instance ToJSON LsRow where
    toJSON r = let namePair = T.pack "path" .= toJSON (filesPath $ rowFiles r)
                   keyvals  = maybe [] keyValueListToJSON (rowKeyVals r)
                   optional = if not (rowUseMetadata r) then [] else catMaybes [
                       fileTypeString (rowFiles r)   >>= \ty     -> Just $ T.pack "fileType" .= toJSON ty,
                       Just $ T.pack "mode" .= toJSON (filesMode $ rowFiles r),
                       Just $ T.pack "size" .= toJSON (filesSize $ rowFiles r),
                       symlinkTarget (rowFiles r)    >>= \target -> Just $ T.pack "symlinkTarget" .= toJSON target,

                       Just $ T.pack "user"  .= toJSON (filesFile_user $ rowFiles r),
                       Just $ T.pack "group" .= toJSON (filesFile_group $ rowFiles r),

                       -- Don't do any special formatting of the mtime - leave that up to the consumer.
                       Just $ T.pack "mtime" .= toJSON (filesMtime $ rowFiles r)
                    ]
               in
                   object $ [namePair] ++ keyvals ++ optional

initRow :: Files -> LsRow
initRow f = LsRow { rowFiles=f,
                    rowUseMetadata=False,
                    rowKeyVals=Nothing }

fileType :: Files -> Maybe Char
fileType Files{filesCs_object=Nothing, ..} = Just 'd'
fileType Files{filesTarget=Just _, ..} = Just 'l'
fileType _ = Nothing

fileTypeString :: Files -> Maybe String
fileTypeString Files{filesCs_object=Nothing, ..} = Just "Directory"
fileTypeString Files{filesTarget=Just _, ..} = Just "Symlink"
fileTypeString _ = Just "File"

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

symlinkTarget :: Files -> Maybe String
symlinkTarget Files{filesTarget=Just x, ..} = Just $ T.unpack x
symlinkTarget _ = Nothing

keyValToLabel :: KeyVal -> Maybe Label
keyValToLabel KeyVal {keyValKey_value=LabelKey x} = Just x
keyValToLabel _                                   = Nothing

runCommand :: T.Text -> [String] -> IO (Either String ())
runCommand db args = do
    (opts, _) <- compilerOpts options defaultLsOptions args "ls"

    when (isNothing $ lsLabelMatches opts) $
        throwIO InvalidLabelError

    printer <- if | lsJSONOutput opts -> return $ liftedPutStrLn . jsonPrinter
                  | lsVerbose opts -> do currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
                                         return $ liftedPutStrLn . verbosePrinter currentYear
                  | otherwise -> return $ liftedPutStrLn . simplePrinter

    runExceptT $ checkAndRunSqlite db $ runConduit $
        -- Grab all the Files, filtering out any whose path does not match what we want.
        filesC .| CL.filter (\f -> T.unpack (filesPath f) =~ lsMatches opts)
        -- Convert them into LsRow records containing only the Files record.
               .| CL.map    initRow
        -- If we were asked for verbose output, add that to the LsRow.
               .| CL.mapM   (\row -> if lsVerbose opts then return row { rowUseMetadata=True }
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
               (ReqArg (\d opts -> opts { lsLabelMatches = readMaybe d }) "LABEL")
               "return only results with the given LABEL",
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { lsMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

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
               (fromMaybe '-' (fileType rowFiles))
               (if rowUseMetadata then T.unpack $ modeAsText $ fromIntegral $ filesMode rowFiles else "--ghost--")
               (T.unpack $ filesFile_user rowFiles) (T.unpack $ filesFile_group rowFiles)
               (if rowUseMetadata then filesSize rowFiles else 0)
               (showTime currentYear $ filesMtime rowFiles)
               (filesPath rowFiles) (maybe "" (" -> " ++) (symlinkTarget rowFiles))
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
        Just (db, _, args) -> do
            unlessM (doesFileExist db) $
                throwIO MissingDBError

            result <- runCommand (T.pack db) args
            whenLeft result print

main :: IO ()
main =
    -- Add handlers for other exception types (IOException, whatever) here.
    runMain `catches` [Handler (\(e :: InspectErrors) -> handleInspectErrors e)]
 where
    -- And then add handlers for the various kinds of InspectErrors here.
    handleInspectErrors :: InspectErrors -> IO ()
    handleInspectErrors InvalidLabelError = do
        putStrLn "Unrecognized file label given.\n"
        putStrLn "Recognized labels:\n"
        forM_ labelDescriptions $ \(l, d) ->
            putStrLn $ "      " ++ l ++ " - " ++ d
        exitFailure

    handleInspectErrors MissingCSError = putStrLn "Content store does not exist\n" >> usage

    handleInspectErrors MissingDBError = putStrLn "Metadata database does not exist\n" >> usage
