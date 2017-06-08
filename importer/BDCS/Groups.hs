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
                   nameToGroupId,
                   nvraToGroupId)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import           BDCS.DB
import qualified BDCS.ReqType as RT

findGroupRequirements :: MonadIO m => Key Groups -> Key Requirements -> SqlPersistT m (Maybe (Key GroupRequirements))
findGroupRequirements groupId reqId = firstResult $
    select $ from $ \r -> do
    where_ $ r ^. GroupRequirementsGroup_id ==. val groupId &&.
             r ^. GroupRequirementsReq_id ==. val reqId
    limit 1
    return $ r ^. GroupRequirementsId

findRequires :: MonadIO m => RT.ReqLanguage -> RT.ReqContext -> RT.ReqStrength -> T.Text -> SqlPersistT m (Maybe (Key Requirements))
findRequires reqLang reqCtx reqStrength reqExpr = firstResult $
    select $ from $ \r -> do
    where_ $ r ^. RequirementsReq_language ==. val reqLang &&.
             r ^. RequirementsReq_context ==. val reqCtx &&.
             r ^. RequirementsReq_strength ==. val reqStrength &&.
             r ^. RequirementsReq_expr ==. val reqExpr
    limit 1
    return $ r ^. RequirementsId

-- Given a group name, return a group id
nameToGroupId :: MonadIO m => T.Text -> SqlPersistT m (Maybe (Key Groups))
nameToGroupId name = firstResult $
    select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` groups) -> do
    on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
             group_keyval ^. GroupKeyValuesGroup_id ==. groups ^. GroupsId
    where_ $ keyval ^. KeyValKey_value ==. val (T.pack "name") &&.
             keyval ^. KeyValVal_value ==. val name &&.
             groups ^. GroupsGroup_type ==. val (T.pack "rpm")
    limit 1
    return $ groups ^. GroupsId

nvraToGroupId :: MonadIO m => (T.Text, Maybe T.Text, T.Text, T.Text, T.Text) -> SqlPersistT m (Maybe (Key Groups))
nvraToGroupId (n, e, v, r, a) = firstResult $
    -- FIXME:  Doesn't work - query's all wrong.  Either adapt the giant thing below to work or remove all
    -- but the name clause from the where_ block.  That will at least make it work as well as nameToGroupId.
    select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` groups) -> do
    on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
             group_keyval ^. GroupKeyValuesGroup_id ==. groups ^. GroupsId
    where_ $ keyval ^. KeyValKey_value ==. val (T.pack "name") &&.  keyval ^. KeyValVal_value ==. val n &&.
             keyval ^. KeyValKey_value ==. val (T.pack "version") &&. keyval ^. KeyValVal_value ==. val v &&.
             keyval ^. KeyValKey_value ==. val (T.pack "release") &&. keyval ^. KeyValVal_value ==. val r &&.
             keyval ^. KeyValKey_value ==. val (T.pack "arch") &&. keyval ^. KeyValVal_value ==. val a &&.
             groups ^. GroupsGroup_type ==. val (T.pack "rpm")
    limit 1
    return $ groups ^. GroupsId

-- FIXME:  Here's the giant query that needs to be adapted.
--         select groups.id
--         from groups
--         join (group_key_values join key_val on group_key_values.key_val_id == key_val.id and key_val.key_value == 'name') name on groups.id == name.group_id
--         join (group_key_values join key_val on group_key_values.key_val_id == key_val.id and key_val.key_value == 'version') ver  on groups.id == ver.group_id
--         join (group_key_values join key_val on group_key_values.key_val_id == key_val.id and key_val.key_value == 'release') rel on groups.id == rel.group_id
--         join (group_key_values join key_val on group_key_values.key_val_id == key_val.id and key_val.key_value == 'arch') arch on groups.id == arch.group_id
--         left outer join (group_key_values join key_val on group_key_values.key_val_id == key_val.id and key_val.key_value == 'epoch') epoch on groups.id == epoch.group_id
--         where name.val_value == :name and
--               epoch.val_value is :epoch and
--               ver.val_value  == :version and
--               rel.val_value  == :release and
--               arch.val_value == :arch
