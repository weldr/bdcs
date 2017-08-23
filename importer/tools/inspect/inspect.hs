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

import           Control.Conditional(unlessM)
import           Control.Monad(when)
import qualified Data.Text as T
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import BDCS.Version

import qualified Commands.Groups as Groups
import qualified Commands.Ls as Ls
import qualified Commands.Nevras as Nevras

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

    let db     = argv !! 0
    let repo   = argv !! 1
    let subcmd = argv !! 2
    let args   = drop 3 argv

    unlessM (doesFileExist db) $ do
        putStrLn "Database does not exist"
        exitFailure

    case subcmd of
        "groups"    -> Groups.runCommand (T.pack db) repo args
        "ls"        -> Ls.runCommand (T.pack db) repo args
        "nevras"    -> Nevras.runCommand (T.pack db) repo args
        _           -> usage
