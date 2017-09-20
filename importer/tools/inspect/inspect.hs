-- Copyright (C) 2017 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Conditional(condM, ifM, otherwiseM)
import Control.Exception(SomeException, catch)
import Control.Monad(forM_, when)
import Control.Monad.Loops(firstM)
import Data.Text(pack, splitOn, unpack)
import System.Directory(doesFileExist)
import System.Environment(getArgs, lookupEnv)
import System.Exit(exitFailure)
import System.FilePath((</>))
import System.Process(callProcess)

import BDCS.Version

import Paths_bdcs(getLibexecDir)

-- A mapping from a subcommand name to a brief description of what it does.
knownSubcommands :: [(String, String)]
knownSubcommands = [
    ("groups",  "List groups (RPM packages, etc.) in the content store"),
    ("ls",      "List files in the content store"),
    ("nevras",  "List NEVRAs of RPM packages in the content store")
 ]

usage :: IO ()
usage = do
    printVersion "inspect"
    putStrLn "Usage: inspect output.db repo subcommand [args ...]"
    putStrLn "- output.db is the path to a metadata database"
    putStrLn "- repo is the path to a content store repo"
    putStrLn "- subcommands:"
    forM_ knownSubcommands $ \(cmd, help) ->
        putStrLn $ "      " ++ cmd ++ " - " ++ help
    exitFailure

getBasePath :: IO FilePath
getBasePath = do
    dir <- getLibexecDir
    return $ dir </> "inspect-"

findInPath :: FilePath -> IO (Maybe FilePath)
findInPath sought = lookupEnv "PATH" >>= \case
    Nothing -> return Nothing
    Just p  -> do let searchPath = splitOn ":" (pack p)
                  fmap (\d -> unpack d </> sought) <$>
                       firstM (\d -> doesFileExist (unpack d </> sought)) searchPath

existsInPath :: FilePath -> IO Bool
existsInPath sought = fmap (/= Nothing) (findInPath sought)

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 3) usage
    let subcmd = argv !! 2
    let subcmdArgs = [head argv, argv !! 1] ++ drop 3 argv

    basePath <- getBasePath

    let cmd1 = basePath ++ subcmd
    let cmd2 = "inspect-" ++ subcmd

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
     tryCallProcess cmd args = catch (callProcess cmd args)
                                     -- We handled the case where an unknown subcommand was
                                     -- given on the command line.  For now, the only other
                                     -- errors possible are when the subcommand ran, but
                                     -- failed for some reason.  Those are handled inside
                                     -- the subcommand.  Just quit.
                                     (\(_ :: SomeException) -> exitFailure)
