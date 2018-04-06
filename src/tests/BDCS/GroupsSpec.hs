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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BDCS.GroupsSpec(spec)
 where

import BDCS.DB(Groups(..))
import BDCS.Groups(groupIdToNevra, nevraToGroupId, getGroupsLike, getGroupsTotal, groups)
import BDCS.GroupKeyValue(insertGroupKeyValue)
import BDCS.KeyType(KeyType(..))

import Control.Monad(void)
import Control.Monad.IO.Class(MonadIO)
import Control.Monad.Logger(NoLoggingT)
import Control.Monad.Trans.Resource(MonadBaseControl, ResourceT)
import Database.Persist.Sql(SqlPersistT, insertKey, toSqlKey)
import Test.Hspec

import Utils(withDb)

spec :: Spec
spec = describe "BDCS.Groups Tests" $ do
    it "nevraToGroupId, has epoch" $
        -- gid <- withNevras $ nevraToGroupId ("hasEpoch", Just "7", "1.0", "1.el7", "x86_64")
        -- gid `shouldBe` Just (toSqlKey 1)
        withNevras (nevraToGroupId ("hasEpoch", Just "7", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Just (toSqlKey 1))

    it "nevraToGroupId, no epoch" $
        withNevras (nevraToGroupId ("noEpoch", Nothing, "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Just (toSqlKey 2))

    it "nevraToGroupId, has epoch, not specified" $
        withNevras (nevraToGroupId ("hasEpoch", Nothing, "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, no epoch, is specified" $
        withNevras (nevraToGroupId ("noEpoch", Just "7", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, has wrong epoch" $
        withNevras (nevraToGroupId ("hasEpoch", Just "8", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong name" $
        withNevras (nevraToGroupId ("missingEpoch", Just "7", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong version" $
        withNevras (nevraToGroupId ("hasEpoch", Just "7", "1.1", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong release" $
        withNevras (nevraToGroupId ("hasEpoch", Just "7", "1.0", "2.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong arch" $
        withNevras (nevraToGroupId ("hasEpoch", Just "7", "1.0", "1.el7", "i686")) >>= (`shouldBe` Nothing)

    it "groupIdToNevra, has epoch" $
      withNevras (groupIdToNevra $ toSqlKey 1) >>= (`shouldBe` Just "hasEpoch-7:1.0-1.el7.x86_64")

    it "groupIdToNevra, no epoch" $
      withNevras (groupIdToNevra $ toSqlKey 2) >>= (`shouldBe` Just "noEpoch-1.0-1.el7.x86_64")

    it "getGroupsLike with % wildcard" $
      withNevras (getGroupsLike Nothing Nothing "%Epoch%") >>= (`shouldBe` ([(toSqlKey 1, "hasEpoch"), (toSqlKey 2, "noEpoch")], 2))

    it "getGroupsLike without % wildcard returns empty list" $
      withNevras (getGroupsLike Nothing Nothing "Epoch") >>= (`shouldBe` ([], 0))

    it "getGroupsLike with % and limited to 2" $
      withNevras (getGroupsLike (Just 0) (Just 1) "%") >>= (`shouldBe` ([(toSqlKey 1, "hasEpoch")], 2))

    it "getGroupsLike with % and offset of 1" $
      withNevras (getGroupsLike (Just 1) (Just 10) "%") >>= (`shouldBe` ([(toSqlKey 2, "noEpoch")], 2))

    it "groups returns all existing records" $
      withNevras groups >>= (`shouldBe` [(toSqlKey 1, "hasEpoch"), (toSqlKey 2, "noEpoch")])

    it "getGroupsTotal" $
      withNevras getGroupsTotal >>= (`shouldBe` 2)

 where
    addNevras :: MonadIO m => SqlPersistT m ()
    addNevras = do
        -- hasEpoch-7:1.0-1.el7.x86_64
        let gid_1 = toSqlKey 1
        insertKey gid_1 $ Groups "hasEpoch" "rpm" Nothing
        void $ insertGroupKeyValue (TextKey "name")    "hasEpoch" Nothing gid_1
        void $ insertGroupKeyValue (TextKey "epoch")   "7"        Nothing gid_1
        void $ insertGroupKeyValue (TextKey "version") "1.0"      Nothing gid_1
        void $ insertGroupKeyValue (TextKey "release") "1.el7"    Nothing gid_1
        void $ insertGroupKeyValue (TextKey "arch")    "x86_64"   Nothing gid_1

        -- noEpoch-1.0-1.el7.x86_64
        let gid_2 = toSqlKey 2
        insertKey gid_2 $ Groups "noEpoch" "rpm" Nothing
        void $ insertGroupKeyValue (TextKey "name")    "noEpoch"  Nothing gid_2
        void $ insertGroupKeyValue (TextKey "version") "1.0"      Nothing gid_2
        void $ insertGroupKeyValue (TextKey "release") "1.el7"    Nothing gid_2
        void $ insertGroupKeyValue (TextKey "arch")    "x86_64"   Nothing gid_2

    withNevras :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
    withNevras action = withDb (addNevras >> action)
