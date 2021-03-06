{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Conditional(unlessM)
import           Control.Exception(Handler(..), catches, throwIO)
import           Control.Monad.Except(runExceptT)
import           Data.Aeson((.=), ToJSON, object, toJSON)
import           Data.Aeson.Encode.Pretty(encodePretty)
import           Data.ByteString.Lazy(toStrict)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Database.Persist.Sqlite(Key)
import           System.Console.GetOpt
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Printf(printf)
import           Text.Regex.PCRE((=~))

import BDCS.DB(Groups(..), KeyVal(..), checkAndRunSqlite)
import BDCS.GroupKeyValue(getKeyValuesForGroup)
import BDCS.Groups(groupsC)
import BDCS.KeyValue(keyValueListToJSON)
import BDCS.Utils.Either(whenLeft)
import BDCS.Version

import Utils.Exceptions(InspectErrors(..))
import Utils.GetOpt(commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)
import Utils.KeyVal(formatKeyValList)

data GroupsOptions = GroupsOptions { grpJSONOutput :: Bool,
                                     grpKeyVals :: Bool,
                                     grpMatches :: String }

defaultGroupsOptions :: GroupsOptions
defaultGroupsOptions = GroupsOptions { grpJSONOutput = False,
                                       grpKeyVals = False,
                                       grpMatches = ".*" }

data GroupsRow = GroupsRow { rowId :: Key Groups,
                             rowKeyVals :: Maybe [KeyVal],
                             rowName :: T.Text }

instance ToJSON GroupsRow where
    toJSON r = let namePair = T.pack "groupName" .= toJSON (rowName r)
                   keyvals  = maybe [] keyValueListToJSON (rowKeyVals r)
               in
                   object (namePair : keyvals)

initRow :: (Key Groups, T.Text) -> GroupsRow
initRow (key, name) = GroupsRow { rowId=key,
                                  rowKeyVals=Nothing,
                                  rowName=name }

runCommand :: T.Text -> FilePath -> [String] -> IO (Either String ())
runCommand db _ args = do
    (opts, _) <- compilerOpts options defaultGroupsOptions args "groups"
    let printer = if grpJSONOutput opts then jsonPrinter else textPrinter

    runExceptT $ checkAndRunSqlite db $ runConduit $
        -- Grab all the Groups, filtering out any whose name does not match what we want.
        groupsC .| CL.filter (\(_, n) -> T.unpack n =~ grpMatches opts)
        -- Convert them into GroupsRow records.
                .| CL.map    initRow
        -- If we were asked for keyval output, add that to the GroupsRow.
                .| CL.mapM   (\row -> if grpKeyVals opts then do
                                          kvs <- getKeyValuesForGroup (rowId row) Nothing
                                          return $ row { rowKeyVals=Just kvs }
                                      else return row)
        -- Finally, pass it to the printer.
                .| CL.mapM_  (liftedPutStrLn . printer)
 where
    options :: [OptDescr (GroupsOptions -> GroupsOptions)]
    options = [
        Option [] ["json"]
               (NoArg (\opts -> opts { grpJSONOutput = True }))
               "format output as JSON",
        Option ['k'] ["keyval"]
               (NoArg (\opts -> opts { grpKeyVals = True }))
               "add key/val pairs to output",
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { grpMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

    -- Note: The docs say that toStrict is an expensive operation that should be used carefully.
    -- I think it's okay here because we are only dealing with one row of output at a time, so
    -- it should be pretty small amounts of data to convert.
    jsonPrinter :: GroupsRow -> T.Text
    jsonPrinter = decodeUtf8 . toStrict . encodePretty

    textPrinter :: GroupsRow -> T.Text
    textPrinter GroupsRow{..} = T.pack $
        printf "%s%s"
               rowName
               (maybe "" formatKeyValList rowKeyVals)

usage :: IO ()
usage = do
    printVersion "inspect-groups"
    putStrLn "Usage: inspect-groups output.db repo [args ...]"
    putStrLn "  List groups (RPM packages, etc.) in the content store"
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
                throwIO MissingDBError

            result <- runCommand (T.pack db) repo args
            whenLeft result (\e -> print $ "error: " ++ e)

main :: IO ()
main =
    -- Add handlers for other exception types (IOException, whatever) here.
    runMain `catches` [Handler (\(e :: InspectErrors) -> handleInspectErrors e)]
 where
    -- And then add handlers for the various kinds of InspectErrors here.
    handleInspectErrors :: InspectErrors -> IO ()
    handleInspectErrors MissingDBError = putStrLn "Metadata database does not exist\n" >> usage

    handleInspectErrors _              = usage
