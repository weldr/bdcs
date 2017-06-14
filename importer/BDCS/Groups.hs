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

{-# LANGUAGE OverloadedStrings #-}

module BDCS.Groups(findGroupRequirements,
                   findRequires,
                   nameToGroupId,
                   nevraToGroupId)
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
    where_ $ keyval ^. KeyValKey_value ==. val "name" &&.
             keyval ^. KeyValVal_value ==. just (val name) &&.
             groups ^. GroupsGroup_type ==. val "rpm"
    limit 1
    return $ groups ^. GroupsId

nevraToGroupId :: MonadIO m => (T.Text, Maybe T.Text, T.Text, T.Text, T.Text) -> SqlPersistT m (Maybe (Key Groups))
nevraToGroupId (n, e, v, r, a) = firstResult $
    -- Each key/val pair to match against is a separate row in key_val, so each one needs to be joined into the
    -- query as if it were a separate table.
    -- The idea here is to create a key_val/group_key_values join for each key_val.key_value we're looking up, and
    -- then join that to the previous key_val/group_key_values in the chain. The one for epoch is an outer join since
    -- epoch is optional.
    -- As far as esqueleto quirks: "on" is applied in reverse order, so the first "on" attaches to the last join, then
    -- the next to the one before that, and so on.
    select $ distinct $ from $ \((gkv_name  `InnerJoin` kv_name) `InnerJoin`
                                 (gkv_ver   `InnerJoin` kv_ver)  `InnerJoin`
                                 (gkv_rel   `InnerJoin` kv_rel)  `InnerJoin`
                                 (gkv_arch  `InnerJoin` kv_arch) `LeftOuterJoin`
                                 (gkv_epoch `InnerJoin` kv_epoch)) -> do
        on     $ kv_epoch ?. KeyValId ==. gkv_epoch ?. GroupKeyValuesKey_val_id &&.
                 kv_epoch ?. KeyValKey_value ==. just (val "epoch")
        on     $ gkv_epoch ?. GroupKeyValuesGroup_id ==. just (gkv_arch ^. GroupKeyValuesGroup_id)
        on     $ kv_arch ^. KeyValId ==. gkv_arch ^. GroupKeyValuesKey_val_id &&.
                 kv_arch ^. KeyValKey_value ==. val "arch"
        on     $ gkv_arch ^. GroupKeyValuesGroup_id ==. gkv_rel ^. GroupKeyValuesGroup_id
        on     $ kv_rel ^. KeyValId ==. gkv_rel ^. GroupKeyValuesKey_val_id &&.
                 kv_rel ^. KeyValKey_value ==. val "release"
        on     $ gkv_rel ^. GroupKeyValuesGroup_id ==. gkv_ver ^. GroupKeyValuesGroup_id
        on     $ kv_ver ^. KeyValId ==. gkv_ver ^. GroupKeyValuesKey_val_id &&.
                 kv_ver ^. KeyValKey_value ==. val "version"
        on     $ gkv_ver ^. GroupKeyValuesGroup_id ==. gkv_name ^. GroupKeyValuesGroup_id
        on     $ kv_name ^. KeyValId ==. gkv_name ^. GroupKeyValuesKey_val_id &&.
                 kv_name ^. KeyValKey_value ==. val "name"
        where_ $ kv_name  ^. KeyValVal_value ==. just (val n) &&.
                 kv_ver   ^. KeyValVal_value ==. just (val v) &&.
                 kv_rel   ^. KeyValVal_value ==. just (val r) &&.
                 kv_arch  ^. KeyValVal_value ==. just (val a) &&.
                 cmpEpoch (kv_epoch ?. KeyValVal_value)
        limit 1
        return $ gkv_name ^. GroupKeyValuesGroup_id
 where
    -- If there is an epoch, do 'key_val.val_value == <epoch>'
    -- If there is no epoch, do 'key_val.val_value is null'
    cmpEpoch = case e of
        Nothing -> isNothing
        Just _  -> (==. just (val e))
