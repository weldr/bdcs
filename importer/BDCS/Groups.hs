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

module BDCS.Groups(findGroupRequirements,
                   findRequires,
                   nameToGroupId)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Data.Maybe(listToMaybe)
import qualified Data.Text as T
import           Database.Esqueleto

import           BDCS.DB
import qualified BDCS.ReqType as RT

findGroupRequirements :: MonadIO m => Key Groups -> Key Requirements -> SqlPersistT m (Maybe (Key GroupRequirements))
findGroupRequirements groupId reqId = do
    ndx  <- select $ from $ \r -> do
            where_ $ r ^. GroupRequirementsGroup_id ==. val groupId &&.
                     r ^. GroupRequirementsReq_id ==. val reqId
            limit 1
            return $ r ^. GroupRequirementsId
    return $ listToMaybe (map unValue ndx)

findRequires :: MonadIO m => RT.ReqLanguage -> RT.ReqContext -> RT.ReqStrength -> T.Text -> SqlPersistT m (Maybe (Key Requirements))
findRequires reqLang reqCtx reqStrength reqExpr = do
    ndx <- select $ from $ \r -> do
           where_ $ r ^. RequirementsReq_language ==. val reqLang &&.
                    r ^. RequirementsReq_context ==. val reqCtx &&.
                    r ^. RequirementsReq_strength ==. val reqStrength &&.
                    r ^. RequirementsReq_expr ==. val reqExpr
           limit 1
           return $ r ^. RequirementsId
    return $ listToMaybe (map unValue ndx)

-- Given a group name, return a group id
nameToGroupId :: MonadIO m => T.Text -> SqlPersistT m (Maybe (Key Groups))
nameToGroupId name = do
    ndx <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` groups) -> do
           on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
                    group_keyval ^. GroupKeyValuesGroup_id ==. groups ^. GroupsId
           where_ $ keyval ^. KeyValKey_value ==. val (T.pack "name") &&.
                    keyval ^. KeyValVal_value ==. val name &&.
                    groups ^. GroupsGroup_type ==. val (T.pack "rpm")
           limit 1
           return $ groups ^. GroupsId
    return $ listToMaybe (map unValue ndx)
