import           Control.Conditional(unlessM)
import           Control.Monad.Except(runExceptT)
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           System.Console.GetOpt
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Regex.PCRE((=~))

import BDCS.DB(checkAndRunSqlite)
import BDCS.Groups(groupsC, groupIdToNevra)
import BDCS.Version

import Utils.Either(whenLeft)
import Utils.GetOpt(OptClass, commandLineArgs, compilerOpts)
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
    result <- runExceptT $ checkAndRunSqlite db $ runConduit $
        groupsC .| CL.map fst
                .| CL.mapMaybeM groupIdToNevra
                .| CL.filter (\g -> T.unpack g =~ nevraMatches opts)
                .| CL.mapM_ liftedPutStrLn
    whenLeft result (\e -> print $ "error: " ++ e)
 where
    options :: [OptDescr (NevrasOptions -> NevrasOptions)]
    options = [
        Option ['m'] ["matches"]
               (ReqArg (\d opts -> opts { nevraMatches = d }) "REGEX")
               "return only results that match REGEX"
     ]

usage :: IO ()
usage = do
    printVersion "inspect-nevras"
    putStrLn "Usage: inspect-nevras output.db repo [args ...]"
    putStrLn "  List NEVRAs of RPM packages in the content store"
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
