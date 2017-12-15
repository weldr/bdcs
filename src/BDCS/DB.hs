{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: BDCS.DB
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- The metadata database schema and miscellaneous database helper functions

module BDCS.DB where

import           Control.Monad(unless)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Logger(NoLoggingT)
import           Control.Monad.Trans.Resource(MonadBaseControl, ResourceT)
import qualified Data.Aeson as Aeson
import           Data.ByteString(ByteString)
import           Data.Int(Int64)
import           Data.Maybe(fromJust, listToMaybe)
import qualified Data.Text as T
import           Data.Time(UTCTime)
import           Database.Esqueleto(Esqueleto, Entity, Key, PersistEntity, PersistField, SqlBackend, SqlPersistT, ToBackendKey, Value,
                                    (==.), entityVal, insert, isNothing, val, unValue)
import           Database.Persist.Sql(rawSql, unSingle)
import           Database.Persist.Sqlite(runSqlite)
import           Database.Persist.TH

import BDCS.KeyType
import BDCS.ReqType

{-# ANN module ("HLint: ignore Use module export list" :: String) #-}
-- Both esqueleto and maybe export isNothing.  I don't want to have to use a qualified import, so
-- we'll just compare things directly to Nothing.
{-# ANN module ("HLint: ignore Use isNothing" :: String) #-}

-- | The database schema version as implemented by this module.  This must match the
-- PRAGMA user_version value in schema.sql, shipped elsewhere in the source.
schemaVersion :: Int64
schemaVersion = 4

-- | Return the version number stored in the database.
getDbVersion :: MonadIO m => SqlPersistT m Int64
getDbVersion = unSingle <$> head <$> rawSql "pragma user_version" []

-- | Verify that the version number stored in the database matches the schema version number
-- implemented by this module.  If there is a version mismatch, throw an error.
checkDbVersion :: (MonadError String m, MonadIO m) => SqlPersistT m ()
checkDbVersion = do
    -- The change from version 3 to version 4 involves changing the content store, so there
    -- is no automatic upgrade path.
    userVersion <- getDbVersion
    unless (userVersion == schemaVersion) $ throwError $
        "Database version " ++ show userVersion ++ " does not match expected version " ++ show schemaVersion ++
            ", please re-import your data"

-- | Like 'Database.Persist.Sqlite.runSqlite', but first checks that the database's schema version
-- matches what is expected.  This prevents running against incompatible database versions.
checkAndRunSqlite :: (MonadError String m, MonadBaseControl IO m, MonadIO m) =>
    T.Text -> SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
checkAndRunSqlite db action = runSqlite db (checkDbVersion >> action)

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
    cs_object ByteString Maybe
    mode Int
    size Int
    target T.Text Maybe
    deriving Eq Show
 SourceFiles
    source_id SourcesId
    file_id FilesId
    deriving Eq Show
 BuildFiles
    build_id BuildsId
    file_id FilesId
    deriving Eq Show
 KeyVal
    key_value KeyType
    val_value T.Text Maybe
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
    build_id BuildsId Maybe
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

instance Aeson.ToJSON KeyVal where
    toJSON kv = let
        v = fmap Aeson.toJSON (keyValVal_value kv)
        e = fmap Aeson.toJSON (keyValExt_value kv)
     in
        if | v == Nothing           -> Aeson.Bool True
           | v == e || e == Nothing -> fromJust v
           | otherwise              -> fromJust e

-- | Run an SQL query, returning the first 'Entity' as a Maybe.  Use this when you
-- want a single row out of the database.
firstEntityResult :: Monad m => m [Entity a] -> m (Maybe a)
firstEntityResult query =
    listToMaybe . map entityVal <$> query

-- | Run an SQL query, returning the first key as a Maybe.  Use this when you want
-- a single index out of the database.
firstKeyResult :: Monad m => m [Value a] -> m (Maybe a)
firstKeyResult query =
    listToMaybe . map unValue <$> query

-- | Like 'maybe', but for keys.  If the key is nothing, return the default value.  Otherwise,
-- run the function on the key and return that value.
maybeKey :: MonadIO m =>
            m b             -- ^ Default value
         -> (t -> m b)      -- ^ A function to run on the key
         -> m (Maybe t)     -- ^ A 'Maybe' key
         -> m b
maybeKey def fn value = value >>= \case
    Nothing -> def
    Just v  -> fn v

-- | Return a query fragment to match a Maybe value.
-- If the value is Nothing, this is equivalent to (column is NULL)
-- If the value is Just x, this is (value == column)
-- Unlike the other Esqueleto operators, the right-hand value is not boxed in a Value,
-- since we need to examine it in order to generate the correct SQL.
--
-- e.g., with a table like:
-- >   create table example (
-- >     id integer primary key,
-- >     value text );
-- you could use an esqueleto query like:
-- >   select $ from $ \example -> do
-- >   where_ $ maybeVal ==? (example ?. ExampleValue)
infix 4 ==?
(==?) :: (PersistField typ, Esqueleto query expr backend) => expr (Value (Maybe typ)) -> Maybe typ -> expr (Value Bool)
(==?) column Nothing = isNothing column
(==?) column value@(Just _) = column ==. val value

-- | Attempt to find a record in some table of the database.  If it exists, return its key.
-- If it doesn't exist, perform some other action and return the key given by that action.
orDo :: MonadIO m => m (Maybe b) -> m b -> m b
orDo findFn doFn =
    findFn >>= maybe doFn return

-- | Attempt to find a record in some table of the database.  If it exists, return its key.
-- If it doesn't exist, insert the given object and return its key.
orInsert :: (MonadIO m, PersistEntity a, ToBackendKey SqlBackend a) => SqlPersistT m (Maybe (Key a)) -> a -> SqlPersistT m (Key a)
orInsert findFn obj =
    findFn >>= maybe (insert obj) return
