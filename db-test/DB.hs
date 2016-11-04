{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB where

import Control.Monad(void)
import Data.ByteString(ByteString)
import Data.Text(pack)
import Data.Time(UTCTime)
import Database.Esqueleto
import Database.Persist.Sqlite(runSqlite)
import Database.Persist.TH
import System.Directory(createDirectoryIfMissing)
import System.FilePath.Posix(takeDirectory)

import FileType

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
 Files
    path String
    digest String
    file_type FileType
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
 |]

initDB ::  FilePath -> IO ()
initDB filename = createDirectoryIfMissing True dirname >>
                  runSqlite (pack filename) (void $ runMigrationSilent migrateAll)
 where
    dirname = takeDirectory filename
