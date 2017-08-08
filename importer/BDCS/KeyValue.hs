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

module BDCS.KeyValue(findKeyValue,
                     insertKeyValue)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import BDCS.KeyType

findKeyValue :: MonadIO m => KeyType -> Maybe T.Text -> Maybe T.Text -> SqlPersistT m (Maybe (Key KeyVal))
findKeyValue k v e = firstResult $
    select $ from $ \kv -> do
    where_ $ kv ^. KeyValKey_value ==. val k &&.
             kv ^. KeyValVal_value ==? v &&.
             kv ^. KeyValExt_value ==? e
    limit 1
    return $ kv ^. KeyValId

insertKeyValue :: MonadIO m => KeyType -> Maybe T.Text -> Maybe T.Text -> SqlPersistT m (Key KeyVal)
insertKeyValue k v e =
    insert (KeyVal k v e)
