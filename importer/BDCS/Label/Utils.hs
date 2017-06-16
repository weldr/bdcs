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

module BDCS.Label.Utils(addLabelKey)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto(Key, SqlPersistT, insert)

import BDCS.DB(Files, FileKeyValues(..), KeyVal, maybeKey)
import BDCS.KeyValue(findKeyValue, insertKeyValue)
import BDCS.KeyType(KeyType(..))
import BDCS.Label.Types(Label)

addLabelKey :: MonadIO m => Key Files -> Label -> Maybe T.Text -> Maybe T.Text -> SqlPersistT m (Key FileKeyValues)
addLabelKey fileId k v e =
    maybeKey (insertKeyValue (LabelKey k) v e >>= \kvId -> insert $ FileKeyValues fileId kvId)
             (insert . FileKeyValues fileId)
             (findKeyValue (LabelKey k) v e)
