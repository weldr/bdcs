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

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Conditional(ifM)
import Control.Exception(SomeException, catch)
import Control.Monad(forM_, when)
import System.Directory(doesFileExist)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Process(callProcess)

import BDCS.Version

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

{-# ANN main "HLint: ignore Use head" #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 3) usage
    let subcmd = argv !! 2
    let subcmdArgs = [head argv, argv !! 1] ++ drop 3 argv

    case subcmd `lookup` knownSubcommands of
        Just _  -> tryCallProcess ("inspect-" ++ subcmd) subcmdArgs
        Nothing -> ifM (doesFileExist ("/usr/libexec/weldr/inspect-" ++ subcmd))
                       (tryCallProcess ("/usr/libexec/weldr/inspect-" ++ subcmd) subcmdArgs)
                       usage
 where
     tryCallProcess cmd args = catch (callProcess cmd args)
                                     (\(_ :: SomeException) -> putStrLn ("subcommand " ++ cmd ++ " does not exist\n") >> usage)
