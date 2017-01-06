-- Copyright (C) 2016 Red Hat, Inc.
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

{-# LANGUAGE TemplateHaskell #-}

module BDCS.FileType where

import Data.Bits((.&.))
import Database.Persist.TH

data FileType = RegularFile | Directory | Socket | SymbolicLink | BlockDevice | CharDevice | FIFO
 deriving(Eq, Read, Show)

derivePersistField "FileType"

-- FIXME:  Really need to figure out a library that has this
getFileType :: Int -> FileType
getFileType mode = case mode .&. 0o170000 of
    0o140000 -> Socket
    0o120000 -> SymbolicLink
    0o100000 -> RegularFile
    0o060000 -> BlockDevice
    0o040000 -> Directory
    0o020000 -> CharDevice
    0o010000 -> FIFO
    _        -> RegularFile
