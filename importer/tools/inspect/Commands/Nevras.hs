module Commands.Nevras(runCommand)
 where

import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Database.Persist.Sqlite(runSqlite)
import           System.Console.GetOpt
import           Text.Regex.PCRE((=~))

import BDCS.Groups(groupsC, groupIdToNevra)

import Utils.GetOpt(OptClass, compilerOpts)
import Utils.IO(liftedPutStrLn)

-- These warnings are coming from options records that only have one field.
-- As options are added, these warnings will go away.  Until then, ignore
-- them.
{-# ANN module "HLint: ignore Use newtype instead of data" #-}

data NevrasOptions = NevrasOptions { nevraMatches :: String }

instance OptClass NevrasOptions

defaultNevrasOptions :: NevrasOptions
defaultNevrasOptions = NevrasOptions { nevraMatches = ".*" }

runCommand :: T.Text -> FilePath -> [String] -> IO ()
runCommand db _ args = do
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
