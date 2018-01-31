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

import Control.Monad(forM_)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import BDCS.Version

import Utils.GetOpt(commandLineArgs)
import Utils.Subcommands(runSubcommand)

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

main :: IO ()
main = commandLineArgs <$> getArgs >>= \case
    Just (db, repo, subcmd:args) ->
        runSubcommand "inspect-" subcmd ([db, repo] ++ args) knownSubcommands usage

    _ -> usage
