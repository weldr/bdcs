{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Conditional(unlessM)
import           Control.Monad.Except(MonadError, runExceptT)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Aeson((.=), ToJSON, object, toJSON)
import           Data.Aeson.Encode.Pretty(encodePretty)
import           Data.ByteString.Lazy(toStrict)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Data.List(intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe(catMaybes, fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
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
import           BDCS.KeyType(asText)
import           BDCS.KeyValue(formatKeyValue)
import           BDCS.Version
import           Utils.Either(whenLeft)
import           Utils.Mode(modeAsText)

import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)

data LsOptions = LsOptions { lsJSONOutput :: Bool,
                             lsKeyVal :: Bool,
                             lsMatches :: String,
                             lsVerbose :: Bool }

instance OptClass LsOptions

defaultLsOptions :: LsOptions
defaultLsOptions = LsOptions { lsJSONOutput = False,
                               lsKeyVal = False,
                               lsMatches = ".*",
                               lsVerbose = False }

data LsRow = LsRow { rowFiles :: Files,
                     rowKeyVals :: Maybe [KeyVal],
                     rowMetadata :: Maybe CS.Object }

instance ToJSON LsRow where
    toJSON r = let namePair = T.pack "path" .= toJSON (filesPath $ rowFiles r)
                   keyvals  = case rowKeyVals r of
                                  Nothing  -> []
                                  -- toJSON on a KeyVal does nothing with the key, so we need to create a list of
                                  -- (key, json) tuples.
                                  Just kvs -> let vals    = map (\kv -> (asText $ keyValKey_value kv, [toJSON kv]))
                                                                kvs
                                                  -- A single group can have many KeyVals with the same key (think about
                                                  -- rpm-provides and requires especially).  We use an intermediate map
                                                  -- to turn it into a list of (key, [json1, json2, ...]) tuples.
                                                  mapping = Map.fromListWith (++) vals
                                                  -- If there's only one KeyVal for a given key, strip the list out before
                                                  -- converting to a json list object.  Otherwise, everything will end up
                                                  -- in a list.
                                                  pairs   = map (\(k, v) -> if length v == 1 then k .= head v else k .= v)
                                                                (Map.toList mapping)
                                               in [T.pack "keyvals" .= object pairs]
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

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db repoPath args = do
    repo <- CS.open repoPath
    (opts, _) <- compilerOpts options defaultLsOptions args "ls"

    printer <- if | lsJSONOutput opts -> return $ liftedPutStrLn . jsonPrinter
                  | lsVerbose opts -> do currentYear <- formatTime defaultTimeLocale "%Y" <$> getCurrentTime
                                         return $ liftedPutStrLn . verbosePrinter currentYear
                  | otherwise -> return $ liftedPutStrLn . simplePrinter

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
        Option [] ["json"]
               (NoArg (\opts -> opts { lsJSONOutput = True }))
               "format output as JSON",
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

    jsonPrinter :: LsRow -> T.Text
    jsonPrinter = decodeUtf8 . toStrict . encodePretty

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
               (fromMaybe '-' (fileType rowMetadata))
               (maybe "--ghost--" (T.unpack . modeAsText . CS.mode) (csMetadata rowMetadata))
               (T.unpack $ filesFile_user rowFiles) (T.unpack $ filesFile_group rowFiles)
               (maybe 0 CS.size (csMetadata rowMetadata))
               (showTime currentYear $ filesMtime rowFiles)
               (filesPath rowFiles) (maybe "" (" -> " ++) (symlinkTarget rowMetadata))
               keyvals
     where
        keyvals = case rowKeyVals of
            Just lst -> printf " [%s]" (intercalate ", " (map (T.unpack . formatKeyValue) lst))
            _ -> ""

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
