import           Control.Conditional(unlessM)
import qualified Data.Text as T
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Database.Persist.Sqlite(runSqlite)
import           System.Console.GetOpt
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Regex.PCRE((=~))

import BDCS.Groups(groupsC)
import BDCS.Version

import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)
import Utils.IO(liftedPutStrLn)

-- These warnings are coming from options records that only have one field.
-- As options are added, these warnings will go away.  Until then, ignore
-- them.
{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data GroupsOptions = GroupsOptions { grpMatches :: String }

instance OptClass GroupsOptions

defaultGroupsOptions :: GroupsOptions
defaultGroupsOptions = GroupsOptions { grpMatches = ".*" }

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db _ args = do
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
