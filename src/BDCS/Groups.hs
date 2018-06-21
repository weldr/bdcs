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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BDCS.Groups(findGroupRequirements,
                   findRequires,
                   getRequires,
                   getGroupId,
                   getGroupIdC,
                   getGroup,
                   getGroupsLike,
                   getGroupsTotal,
                   getGroupRequirements,
                   getRequirementsForGroup,
                   groups,
                   groupsC,
                   groupIdToNevra,
                   nameToGroupIds,
                   nevraToGroupId)
 where

import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Trans.Resource(MonadResource)
import           Data.Bifunctor(bimap)
import           Data.Conduit((.|), Conduit, Source)
import qualified Data.Conduit.List as CL
import           Data.Int(Int64)
import qualified Data.Text as T
import           Database.Esqueleto

import           BDCS.DB
import           BDCS.GroupKeyValue(getValueForGroup)
import           BDCS.KeyType
import qualified BDCS.ReqType as RT
import           BDCS.RPM.Utils(splitFilename)

{-# ANN findGroupRequirements ("HLint: ignore Use ." :: String) #-}
{-# ANN findRequires ("HLint: ignore Use ." :: String) #-}
{-# ANN getRequirementsForGroup ("HLint: ignore Use ." :: String) #-}
{-# ANN nameToGroupIds ("HLint: ignore Use ." :: String) #-}
{-# ANN nevraToGroupId ("HLint: ignore Use ." :: String) #-}

findGroupRequirements :: MonadIO m => Key Groups -> Key Requirements -> SqlPersistT m (Maybe (Key GroupRequirements))
findGroupRequirements groupId reqId = firstKeyResult $
    select $ from $ \r -> do
    where_ $ r ^. GroupRequirementsGroup_id ==. val groupId &&.
             r ^. GroupRequirementsReq_id ==. val reqId
    limit 1
    return $ r ^. GroupRequirementsId

getGroupRequirements :: MonadIO m => Key GroupRequirements -> SqlPersistT m (Maybe GroupRequirements)
getGroupRequirements key = firstEntityResult $
    select $ from $ \r -> do
    where_ $ r ^. GroupRequirementsId ==. val key
    limit 1
    return r

findRequires :: MonadIO m => RT.ReqLanguage -> RT.ReqContext -> RT.ReqStrength -> T.Text -> SqlPersistT m (Maybe (Key Requirements))
findRequires reqLang reqCtx reqStrength reqExpr = firstKeyResult $
    select $ from $ \r -> do
    where_ $ r ^. RequirementsReq_language ==. val reqLang &&.
             r ^. RequirementsReq_context ==. val reqCtx &&.
             r ^. RequirementsReq_strength ==. val reqStrength &&.
             r ^. RequirementsReq_expr ==. val reqExpr
    limit 1
    return $ r ^. RequirementsId

getRequires :: MonadIO m => Key Requirements -> SqlPersistT m (Maybe Requirements)
getRequires key = firstEntityResult $
    select $ from $ \r -> do
    where_ $ r ^. RequirementsId ==. val key
    limit 1
    return r

getGroupId :: (MonadError String m, MonadIO m) => T.Text -> SqlPersistT m (Key Groups)
getGroupId thing =
    nevraToGroupId (splitFilename thing) >>= \case
        Just gid -> return gid
        Nothing  -> throwError $ "No such group " ++ T.unpack thing

getGroupIdC :: (MonadError String m, MonadIO m) => Conduit T.Text (SqlPersistT m) (Key Groups)
getGroupIdC = CL.mapM getGroupId

getGroup :: MonadIO m => Key Groups -> SqlPersistT m (Maybe Groups)
getGroup key = firstEntityResult $
    select $ from $ \group -> do
    where_ $ group ^. GroupsId ==. val key
    limit 1
    return group

-- | Get the groups matching a name
-- Optionally limit the results with limit and offset
-- Also returns the total number of results, before offset and limit are applied
getGroupsLike :: MonadIO m => Maybe Int64 -> Maybe Int64 -> T.Text -> SqlPersistT m ([(Key Groups, T.Text)], Int64)
getGroupsLike (Just ofst) (Just lmt) name = do
    results <- select $ from $ \group -> do
               where_ $ group ^. GroupsName `like` val name
               orderBy [asc (group ^. GroupsName)]
               offset ofst
               limit lmt
               return  (group ^. GroupsId, group ^. GroupsName)
    total <- firstListResult $
             select $ from $ \group -> do
             where_ $ group ^. GroupsName `like` val name
             return countRows
    return (map (bimap unValue unValue) results, total)

getGroupsLike _ _ name = do
    results <- select $ from $ \group -> do
               where_ $ group ^. GroupsName `like` val name
               orderBy [asc (group ^. GroupsName)]
               return  (group ^. GroupsId, group ^. GroupsName)
    total <- firstListResult $
             select $ from $ \group -> do
             where_ $ group ^. GroupsName `like` val name
             return countRows
    return (map (bimap unValue unValue) results, total)

-- | Return the total number of groups
getGroupsTotal :: MonadIO m => SqlPersistT m Int64
getGroupsTotal = firstListResult $
    select . from $ \(_ :: SqlExpr (Entity Groups)) -> return countRows

groups :: MonadIO m => SqlPersistT m [(Key Groups, T.Text)]
groups = do
    results <- select  $ from $ \group -> do
               orderBy [asc (group ^. GroupsName)]
               return  (group ^. GroupsId, group ^. GroupsName)
    return $ map (bimap unValue unValue) results

groupsC :: MonadResource m => Source (SqlPersistT m) (Key Groups, T.Text)
groupsC = do
    let source = selectSource $ from $ \group -> do
                 orderBy      [asc (group ^. GroupsName)]
                 return       (group ^. GroupsId, group ^. GroupsName)
    source .| CL.map (bimap unValue unValue)

groupIdToNevra :: MonadIO m => Key Groups -> SqlPersistT m (Maybe T.Text)
groupIdToNevra groupId = do
    n <- getValueForGroup groupId (TextKey "name")
    e <- getValueForGroup groupId (TextKey "epoch")
    v <- getValueForGroup groupId (TextKey "version")
    r <- getValueForGroup groupId (TextKey "release")
    a <- getValueForGroup groupId (TextKey "arch")

    case (n, v, r, a) of
        (Just n', Just v', Just r', Just a') -> return $ Just $ T.concat [n', "-", epoch e, v', "-", r', ".", a']
        _                                    -> return Nothing
  where
    epoch :: Maybe T.Text -> T.Text
    epoch (Just e) = e `T.append` ":"
    epoch Nothing  = ""

getRequirementsForGroup :: MonadIO m => Key Groups -> RT.ReqContext -> RT.ReqStrength -> SqlPersistT m [Requirements]
getRequirementsForGroup groupId context strength = do
    vals <- select $ from $ \(reqs `InnerJoin` groupreqs) -> do
            on     $ reqs ^. RequirementsId ==. groupreqs ^. GroupRequirementsReq_id
            where_ $ groupreqs ^. GroupRequirementsGroup_id ==. val groupId &&.
                     reqs ^. RequirementsReq_context ==. val context &&.
                     reqs ^. RequirementsReq_strength ==. val strength
            return   reqs
    return $ map entityVal vals

-- Given a group name, return a list of matching group ids
nameToGroupIds :: MonadIO m => T.Text -> SqlPersistT m [Key Groups]
nameToGroupIds name = do
    result <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` group) -> do
              on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
                       group_keyval ^. GroupKeyValuesGroup_id ==. group ^. GroupsId
              where_ $ keyval ^. KeyValKey_value ==. val (TextKey "name") &&.
                       keyval ^. KeyValVal_value ==. just (val name) &&.
                       group ^. GroupsGroup_type ==. val "rpm"
              return $ group ^. GroupsId
    return $ map unValue result

nevraToGroupId :: MonadIO m => (T.Text, Maybe T.Text, T.Text, T.Text, T.Text) -> SqlPersistT m (Maybe (Key Groups))
nevraToGroupId (n, e, v, r, a) = firstKeyResult $
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
        on     $ kv_epoch ^. KeyValId ==. gkv_epoch ^. GroupKeyValuesKey_val_id &&.
                 kv_epoch ^. KeyValKey_value ==. val (TextKey "epoch")
        on     $ gkv_epoch ^. GroupKeyValuesGroup_id ==. gkv_arch ^. GroupKeyValuesGroup_id
        on     $ kv_arch ^. KeyValId ==. gkv_arch ^. GroupKeyValuesKey_val_id &&.
                 kv_arch ^. KeyValKey_value ==. val (TextKey "arch")
        on     $ gkv_arch ^. GroupKeyValuesGroup_id ==. gkv_rel ^. GroupKeyValuesGroup_id
        on     $ kv_rel ^. KeyValId ==. gkv_rel ^. GroupKeyValuesKey_val_id &&.
                 kv_rel ^. KeyValKey_value ==. val (TextKey "release")
        on     $ gkv_rel ^. GroupKeyValuesGroup_id ==. gkv_ver ^. GroupKeyValuesGroup_id
        on     $ kv_ver ^. KeyValId ==. gkv_ver ^. GroupKeyValuesKey_val_id &&.
                 kv_ver ^. KeyValKey_value ==. val (TextKey "version")
        on     $ gkv_ver ^. GroupKeyValuesGroup_id ==. gkv_name ^. GroupKeyValuesGroup_id
        on     $ kv_name ^. KeyValId ==. gkv_name ^. GroupKeyValuesKey_val_id &&.
                 kv_name ^. KeyValKey_value ==. val (TextKey "name")
        where_ $ kv_name  ^. KeyValVal_value ==. just (val n) &&.
                 kv_ver   ^. KeyValVal_value ==. just (val v) &&.
                 kv_rel   ^. KeyValVal_value ==. just (val r) &&.
                 kv_arch  ^. KeyValVal_value ==. just (val a) &&.
                 kv_epoch ^. KeyValVal_value ==? e
        limit 1
        return $ gkv_name ^. GroupKeyValuesGroup_id
