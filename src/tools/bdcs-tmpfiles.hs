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

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv /= 2) usage

    let cfg = argv !! 0
    let dir = argv !! 1
    createDirectoryIfMissing True dir
    setupFilesystem dir cfg
