{-# LANGUAGE LambdaCase #-}

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

import Control.Monad.Logger(runNoLoggingT)
import System.Directory(createDirectoryIfMissing)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import BDCS.Export.TmpFiles(setupFilesystem)
import BDCS.Version

usage :: IO ()
usage = do
    printVersion "bdcs-tmpfiles"
    putStrLn "Usage: bdcs-tmpfiles <config> <dest>"
    putStrLn "       config should be a systemd tmpfiles.d configuration file."
    putStrLn "       dest should be a destination directory."
    exitFailure

main :: IO ()
main = getArgs >>= \case
    cfg:dir:_ -> do createDirectoryIfMissing True dir
                    runNoLoggingT $ setupFilesystem dir cfg
    _         -> usage
