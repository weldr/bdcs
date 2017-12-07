{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Conditional(unlessM)
import           Control.Exception(Handler(..), catches, throw)
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
import BDCS.Utils.Either(whenLeft)
import BDCS.Version

import Utils.Exceptions(InspectErrors(..))
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

runCommand :: T.Text -> FilePath -> [String] -> IO (Either String ())
runCommand db _ args = do
    (opts, _) <- compilerOpts options defaultNevrasOptions args "nevras"
    runExceptT $ checkAndRunSqlite db $ runConduit $
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

usage :: IO ()
usage = do
    printVersion "inspect-nevras"
    putStrLn "Usage: inspect-nevras output.db repo [args ...]"
    putStrLn "  List NEVRAs of RPM packages in the content store"
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
