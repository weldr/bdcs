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

import Control.Monad(forM_)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import BDCS.Version

import Utils.Subcommands(runSubcommand)

-- A mapping from a subcommand name to a brief description of what it does.
knownSubcommands :: [(String, String)]
knownSubcommands = [
    ("depsolve", "print a list of all the dependencies of some object"),
    ("export",   "extract objects from the content store and build an image"),
    ("import",   "load packages into the content store"),
    ("inspect",  "inspect the contents of the content store in various ways")
 ]

usage :: IO ()
usage = do
    printVersion "bdcs"
    putStrLn "Usage: bdcs subcommand [args ...]"
    putStrLn "- subcommands:"
    forM_ knownSubcommands $ \(cmd, help) ->
        putStrLn $ "      " ++ cmd ++ " - " ++ help
    exitFailure

main :: IO ()
main = do
    argv <- getArgs

    case argv of
        subcmd:subcmdArgs -> runSubcommand "bdcs-" subcmd subcmdArgs knownSubcommands usage
        _                 -> usage
