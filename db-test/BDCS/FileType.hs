-- Copyright (C) 2016-2017 Red Hat, Inc.
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

module BDCS.FileType where

import Control.Monad.IO.Class(MonadIO)
import Data.Bits((.&.))
import Data.Maybe(listToMaybe)
import Database.Esqueleto

import BDCS.DB

getFileTypeId :: MonadIO m => String -> SqlPersistT m (Maybe (Key FileTypes))
getFileTypeId name = do
    ndx <- select $ from $ \filety -> do
           where_ $ filety ^. FileTypesFile_type ==. val name
           limit 1
           return $ filety ^. FileTypesId
    return $ listToMaybe (map unValue ndx)

getFileType :: MonadIO m => Int -> SqlPersistT m (Maybe (Key FileTypes))
getFileType mode = case mode .&. 0o170000 of
    0o140000 -> getFileTypeId "socket"
    0o120000 -> getFileTypeId "symbolic link"
    0o100000 -> getFileTypeId "regular file"
    0o060000 -> getFileTypeId "block device"
    0o040000 -> getFileTypeId "directory"
    0o020000 -> getFileTypeId "char device"
    0o010000 -> getFileTypeId "FIFO"
    _        -> getFileTypeId "regular file"
