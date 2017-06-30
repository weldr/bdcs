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
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils(fakeKey,
             withDb)
 where

import Control.Monad(void)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, ResourceT)
import Control.Monad.Logger(NoLoggingT)
import Database.Persist.Sql(Key, SqlBackend, SqlPersistT, ToBackendKey, insertKey, runMigrationSilent, toSqlKey)
import Database.Persist.Sqlite(runSqlite)

import BDCS.DB
import BDCS.GroupKeyValue
import BDCS.KeyType

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

fakeKey :: ToBackendKey SqlBackend a => Key a
fakeKey = toSqlKey 0    

-- Run a database action within an in-memory test database
withDb :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
withDb action = runSqlite ":memory:" (initDb >> action)
 where
    initDb :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT m ()
    initDb = do
        void $ runMigrationSilent migrateAll

        -- For nevraToGroupId:
        -- hasEpoch-7:1.0-1.el7.x86_64
        let gid_1 = toSqlKey 1
        insertKey gid_1 $ Groups "hasEpoch" "rpm"
        void $ insertGroupKeyValue (TextKey "name")    "hasEpoch" Nothing gid_1
        void $ insertGroupKeyValue (TextKey "epoch")   "7"        Nothing gid_1
        void $ insertGroupKeyValue (TextKey "version") "1.0"      Nothing gid_1
        void $ insertGroupKeyValue (TextKey "release") "1.el7"    Nothing gid_1
        void $ insertGroupKeyValue (TextKey "arch")    "x86_64"   Nothing gid_1

        -- noEpoch-1.0-1.el7.x86_64
        let gid_2 = toSqlKey 2
        insertKey gid_2 $ Groups "noEpoch" "rpm"
        void $ insertGroupKeyValue (TextKey "name")    "noEpoch"  Nothing gid_2
        void $ insertGroupKeyValue (TextKey "version") "1.0"      Nothing gid_2
        void $ insertGroupKeyValue (TextKey "release") "1.el7"    Nothing gid_2
        void $ insertGroupKeyValue (TextKey "arch")    "x86_64"   Nothing gid_2
