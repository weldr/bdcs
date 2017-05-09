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

{-# LANGUAGE LambdaCase #-}

module BDCS.GroupKeyValue(insertGroupKeyValue)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import BDCS.KeyValue(findKeyValue, insertKeyValue)

insertGroupKeyValue :: MonadIO m => T.Text -> T.Text -> Maybe T.Text -> Key Groups -> SqlPersistT m (Key GroupKeyValues)
insertGroupKeyValue k v e groupId =
    findKeyValue k v e >>= \case
        Nothing -> insertKeyValue k v e >>= \kvId -> insert $ GroupKeyValues groupId kvId
        Just kv -> insert $ GroupKeyValues groupId kv
