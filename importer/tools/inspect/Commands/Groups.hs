{-# LANGUAGE RecordWildCards #-}

import           Control.Conditional(unlessM)
import           Data.List(intercalate)
import qualified Data.Text as T
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Database.Persist.Sqlite(Key, runSqlite)
import           System.Console.GetOpt
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Printf(printf)
import           Text.Regex.PCRE((=~))

import BDCS.DB(Groups(..), KeyVal)
import BDCS.GroupKeyValue(getKeyValuesForGroup)
import BDCS.Groups(groupsC)
import BDCS.KeyValue(formatKeyValue)
import BDCS.Version

import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)

data GroupsOptions = GroupsOptions { grpKeyVals :: Bool,
                                     grpMatches :: String }

instance OptClass GroupsOptions

defaultGroupsOptions :: GroupsOptions
defaultGroupsOptions = GroupsOptions { grpKeyVals = False,
                                       grpMatches = ".*" }

data GroupsRow = GroupsRow { rowId :: Key Groups,
                             rowKeyVals :: Maybe [KeyVal],
                             rowName :: T.Text }

initRow :: (Key Groups, T.Text) -> GroupsRow
initRow (key, name) = GroupsRow { rowId=key,
                                  rowKeyVals=Nothing,
                                  rowName=name }

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db _ args = do
    (opts, _) <- compilerOpts options defaultGroupsOptions args "groups"
    runSqlite db $ runConduit $
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
        Option ['k'] ["keyval"]
               (NoArg (\opts -> opts { grpKeyVals = True }))
               "add key/val pairs to output",
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { grpMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

    printer :: GroupsRow -> T.Text
    printer GroupsRow{..} = T.pack $
        printf "%s%s"
               rowName
               keyvals
     where
        keyvals = case rowKeyVals of
            Just lst -> printf " [%s]" (intercalate ", " (map (T.unpack . formatKeyValue) lst))
            _ -> ""

usage :: IO ()
usage = do
    printVersion "inspect-groups"
    putStrLn "Usage: inspect-groups output.db repo [args ...]"
    putStrLn "  List groups (RPM packages, etc.) in the content store"
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

            runCommand (T.pack db) repo args
