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

module BDCS.GroupKeyValue(getKeyValuesForGroup,
                          getValueForGroup,
                          insertGroupKeyValue)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Data.Maybe(listToMaybe, mapMaybe)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import BDCS.KeyType
import BDCS.KeyValue(findKeyValue, insertKeyValue)

insertGroupKeyValue :: MonadIO m => KeyType -> T.Text -> Maybe T.Text -> Key Groups -> SqlPersistT m (Key GroupKeyValues)
insertGroupKeyValue k v e groupId =
    maybeKey (insertKeyValue k (Just v) e >>= \kvId -> insert $ GroupKeyValues groupId kvId)
             (insert . GroupKeyValues groupId)
             (findKeyValue k (Just v) e)

-- Given a group id and a key, return a list of the matching values
getKeyValuesForGroup :: MonadIO m => Key Groups -> KeyType -> SqlPersistT m [KeyVal]
getKeyValuesForGroup groupId key = do
    vals <- select $ from $ \(keyval `InnerJoin` group_keyval) -> do
            on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id
            where_ $ group_keyval ^. GroupKeyValuesGroup_id ==. val groupId &&.
                     keyval ^. KeyValKey_value ==. val key
            return   keyval
    return $ map entityVal vals

-- Fetch the value for a key/val pair that is expected to occur only once
getValueForGroup :: MonadIO m => Key Groups -> KeyType -> SqlPersistT m (Maybe T.Text)
getValueForGroup groupId key = listToMaybe <$> mapMaybe keyValVal_value <$> getKeyValuesForGroup groupId key
