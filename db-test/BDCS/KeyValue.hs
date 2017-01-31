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

import Control.Monad.IO.Class(MonadIO)
import Database.Esqueleto
import Data.Maybe(listToMaybe, maybe)

import BDCS.DB

findKeyValue :: MonadIO m => String -> String -> Maybe String -> SqlPersistT m (Maybe (Key KeyVal))
findKeyValue k v e = do
    ndx <- select $ from $ \kv -> do
           where_ (kv ^. KeyValKey_value ==. val k &&.
                   kv ^. KeyValVal_value ==. val v &&.
                   kv ^. KeyValExt_value ==. val e)
           limit 1
           return (kv ^. KeyValId)
    return $ listToMaybe (map unValue ndx)

insertKeyValue :: MonadIO m => String -> String -> Maybe String -> SqlPersistT m (Key KeyVal)
insertKeyValue k v e =
    insert (KeyVal k v e)
