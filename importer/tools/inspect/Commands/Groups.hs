module Commands.Groups(runCommand)
 where

import qualified Data.Text as T
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Database.Persist.Sqlite(runSqlite)
import           System.Console.GetOpt
import           Text.Regex.PCRE((=~))

import BDCS.Groups(groupsC)

import Utils.GetOpt(OptClass, compilerOpts)
import Utils.IO(liftedPutStrLn)

-- These warnings are coming from options records that only have one field.
-- As options are added, these warnings will go away.  Until then, ignore
-- them.
{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data GroupsOptions = GroupsOptions { grpMatches :: String }

instance OptClass GroupsOptions

defaultGroupsOptions :: GroupsOptions
defaultGroupsOptions = GroupsOptions { grpMatches = ".*" }

runCommand :: T.Text -> [String] -> IO ()
runCommand db args = do
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

