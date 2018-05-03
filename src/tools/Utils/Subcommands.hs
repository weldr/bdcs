{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Subcommands(runSubcommand)
 where

import           Control.Conditional(condM, ifM, otherwiseM)
import qualified Control.Exception.Lifted as CEL
import           Control.Monad.Loops(firstM)
import           Data.Text(pack, splitOn, unpack)
import           System.Directory(doesFileExist)
import           System.Environment(lookupEnv)
import           System.Exit(exitFailure)
import           System.FilePath((</>))
import           System.Process.Typed(proc, runProcess_)

import Paths_bdcs(getLibexecDir)

getBasePath :: String -> IO FilePath
getBasePath masterCmdName = do
    dir <- getLibexecDir
    return $ dir </> masterCmdName

findInPath :: FilePath -> IO (Maybe FilePath)
findInPath sought = lookupEnv "PATH" >>= \case
    Nothing -> return Nothing
    Just p  -> do let searchPath = splitOn ":" (pack p)
                  fmap (\d -> unpack d </> sought) <$>
                       firstM (\d -> doesFileExist (unpack d </> sought)) searchPath

existsInPath :: FilePath -> IO Bool
existsInPath sought = fmap (/= Nothing) (findInPath sought)

runSubcommand :: String -> String -> [String] -> [(String, String)] -> IO () -> IO ()
runSubcommand basecmdPrefix subcmd subcmdArgs knownSubcommands usage = do
    basePath <- getBasePath basecmdPrefix

    let cmd1 = basePath ++ subcmd
    let cmd2 = basecmdPrefix ++ subcmd

    case subcmd `lookup` knownSubcommands of
        -- This is a subcommand we have built-in knowledge of.  For ease of development, it
        -- could be located in a couple different places:  Installed in /usr/libexec/weldr,
        -- or in the $PATH.  The latter allows us to run "PATH=$PATH:dist/build/... cabal run"
        -- without having to run "cabal install" every time we want to test something.
        --
        -- While tryCallProcess will search the $PATH itself, we need to know if the file
        -- exists somewhere in the $PATH first.  This allows us to separate file not found
        -- errors from the command failing.
        Just _  -> condM [(doesFileExist cmd1, tryCallProcess cmd1 subcmdArgs),
                          (existsInPath cmd1,  tryCallProcess cmd1 subcmdArgs),
                          (doesFileExist cmd2, tryCallProcess cmd2 subcmdArgs),
                          (existsInPath cmd2,  tryCallProcess cmd2 subcmdArgs),
                          (otherwiseM,         putStrLn ("subcommand " ++ subcmd ++ " does not exist\n") >> usage)]

        -- This is a subcommand we know nothing about.  Check to see if it exists in
        -- /usr/libexec/weldr, since it could have been installed by a third party.  If so,
        -- run that.  If not, display an error message and quit.
        Nothing -> ifM (doesFileExist cmd1)
                       (tryCallProcess cmd1 subcmdArgs)
                       (putStrLn ("subcommand " ++ subcmd ++ " does not exist\n") >> usage)
 where
     tryCallProcess cmd args = CEL.catch (runProcess_ (proc cmd args))
                                         -- We handled the case where an unknown subcommand was
                                         -- given on the command line.  For now, the only other
                                         -- errors possible are when the subcommand ran, but
                                         -- failed for some reason.  Those are handled inside
                                         -- the subcommand.  Just quit.
                                         (\(_ :: CEL.SomeException) -> exitFailure)
