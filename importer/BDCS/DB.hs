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

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module BDCS.DB where

import           Data.ByteString(ByteString)
import qualified Data.Text as T
import           Data.Time(UTCTime)
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
 |]
