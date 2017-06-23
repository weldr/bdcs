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

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module BDCS.DB where

import           Control.Monad.IO.Class(MonadIO)
import           Data.ByteString(ByteString)
import           Data.Maybe(listToMaybe)
import qualified Data.Text as T
import           Data.Time(UTCTime)
import           Database.Esqueleto(Key, PersistEntity, SqlBackend, SqlPersistT, ToBackendKey, Value, insert,  unValue)
import           Database.Persist.TH

import BDCS.ReqType

{-# ANN module ("HLint: ignore Use module export list" :: String) #-}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
 Projects
    name T.Text
    summary T.Text
    description T.Text
    homepage T.Text Maybe
    upstream_vcs T.Text
    NameKey name
    deriving Eq Show
 Sources
    project_id ProjectsId
    license T.Text
    version T.Text
    source_ref T.Text
    deriving Eq Show
 Builds
    source_id SourcesId
    epoch Int default=0
    release T.Text
    arch T.Text
    build_time UTCTime
    changelog ByteString
    build_config_ref T.Text
    build_env_ref T.Text
    deriving Eq Show
 BuildSignatures
    build_id BuildsId
    signature_type T.Text
    signature_data ByteString
    deriving Eq Show
 Files
    path T.Text
    file_user T.Text
    file_group T.Text
    mtime Int
    cs_object T.Text Maybe
    deriving Eq Show
 BuildFiles
    build_id BuildsId
    file_id FilesId
    deriving Eq Show
 KeyVal
    key_value T.Text
    val_value T.Text
    ext_value T.Text Maybe
    deriving Eq Show
 ProjectKeyValues
    package_id ProjectsId
    key_val_id KeyValId
    deriving Eq Show
 SourceKeyValues
    source_id SourcesId
    key_val_id KeyValId
    deriving Eq Show
 BuildKeyValues
    build_id BuildsId
    key_val_id KeyValId
    deriving Eq Show
 FileKeyValues
    file_id FilesId
    key_val_id KeyValId
    deriving Eq Show
 Groups
    name T.Text
    group_type T.Text
    deriving Eq Show
 GroupFiles
    group_id GroupsId
    file_id FilesId
    deriving Eq Show
 GroupGroups
    parent_group_id GroupsId
    child_group_id GroupsId
    deriving Eq Show
 GroupKeyValues
    group_id GroupsId
    key_val_id KeyValId
    deriving Eq Show
 Requirements
    req_language ReqLanguage
    req_context ReqContext
    req_strength ReqStrength
    req_expr T.Text
    deriving Eq Show
 GroupRequirements
    group_id GroupsId
    req_id RequirementsId
    deriving Eq Show
 Scripts
    ty T.Text
    body T.Text
    trigger_prog T.Text Maybe
    trigger_index Int Maybe
    trigger_name T.Text Maybe
    trigger_version T.Text Maybe
    trigger_flags Int Maybe
    deriving Eq Show
 GroupScripts
    group_id GroupsId
    script_id ScriptsId
    deriving Eq Show
 |]

-- Run a sql query, returning the first value as a Maybe.
firstResult :: Monad m => m [Value a] -> m (Maybe a)
firstResult query = do
    ret <- query
    return $ listToMaybe (map unValue ret)

-- Like maybe, but for keys - take a default value, a function, potentially the key given
-- by some other database query.  If the key is Nothing, return the default value.  Otherwise,
-- run the function on the key and return that value.
maybeKey :: MonadIO m => m b -> (t -> m b) -> m (Maybe t) -> m b
maybeKey def fn val = val >>= \case
    Nothing -> def
    Just v  -> fn v

-- Attempt to find a record in some table of the database.  If it exists, return its key.
-- If it doesn't exist, perform some other action and return the key given by that action.
orDo :: MonadIO m => m (Maybe b) -> m b -> m b
orDo findFn doFn =
    findFn >>= maybe doFn return

-- Attempt to find a record in some table of the database.  If it exists, return its key.
-- If it doesn't exist, insert the given object and return its key.
orInsert :: (MonadIO m, PersistEntity a, ToBackendKey SqlBackend a) => SqlPersistT m (Maybe (Key a)) -> a -> SqlPersistT m (Key a)
orInsert findFn obj =
    findFn >>= maybe (insert obj) return
