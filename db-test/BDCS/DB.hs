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

import Data.ByteString(ByteString)
import Data.Time(UTCTime)
import Database.Persist.TH

import BDCS.ReqType

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
 Projects
    name String
    summary String
    description String
    homepage String Maybe
    upstream_vcs String
    NameKey name
    deriving Eq Show
 Sources
    project_id ProjectsId
    license String
    version String
    source_ref String
    deriving Eq Show
 Builds
    source_id SourcesId
    epoch Int default=0
    release String
    arch String
    build_time UTCTime
    changelog ByteString
    build_config_ref String
    build_env_ref String
    deriving Eq Show
 BuildSignatures
    build_id BuildsId
    signature_type String
    signature_data ByteString
    deriving Eq Show
 FileTypes
    file_type String
    deriving Eq Show
 Files
    path String
    digest String
    file_type_id FileTypesId
    file_mode Int
    file_user String
    file_group String
    file_size Int
    mtime Int
    symlink_target String Maybe
    deriving Eq Show
 BuildFiles
    build_id BuildsId
    file_id FilesId
    deriving Eq Show
 KeyVal
    key_value String
    val_value String
    ext_value String Maybe
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
    name String
    group_type String
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
    req_expr String
    deriving Eq Show
 GroupRequirements
    group_id GroupsId
    req_id RequirementsId
    deriving Eq Show
 |]
