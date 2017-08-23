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

import Control.Monad(when)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.Process(callProcess)

import BDCS.Version

usage :: IO ()
usage = do
    printVersion "inspect"
    putStrLn "Usage: inspect output.db repo subcommand [args ...]"
    putStrLn "- output.db is the path to a metadata database"
    putStrLn "- repo is the path to a content store repo"
    putStrLn "- subcommands:"
    putStrLn "      groups - List groups (packages, etc.)"
    putStrLn "      ls     - List files"
    putStrLn "      nevras - List NEVRAs of RPM packages"
    exitFailure

{-# ANN main "HLint: ignore Use head" #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 3) usage
    let subcmd = argv !! 2
    let subcmdArgs = [head argv, argv !! 1] ++ drop 3 argv

    case subcmd of
        "groups"    -> callProcess "inspect-groups" subcmdArgs
        "ls"        -> callProcess "inspect-ls" subcmdArgs
        "nevras"    -> callProcess "inspect-nevras" subcmdArgs
        _           -> usage
