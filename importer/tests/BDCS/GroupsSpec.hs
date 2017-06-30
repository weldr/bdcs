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

{-# LANGUAGE OverloadedStrings #-}

module BDCS.GroupsSpec(spec)
 where

import BDCS.Groups(nevraToGroupId)
import BDCS.DB

import Database.Persist.Sql(toSqlKey)
import Test.Hspec

import Utils(withDb)

spec :: Spec
spec = describe "BDCS.Groups Tests" $ do
    it "nevraToGroupId, has epoch" $
        -- gid <- withDb $ nevraToGroupId ("hasEpoch", Just "7", "1.0", "1.el7", "x86_64")
        -- gid `shouldBe` Just (toSqlKey 1)
        withDb (nevraToGroupId ("hasEpoch", Just "7", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Just (toSqlKey 1))

    it "nevraToGroupId, no epoch" $
        withDb (nevraToGroupId ("noEpoch", Nothing, "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Just (toSqlKey 2))

    it "nevraToGroupId, has epoch, not specified" $
        withDb (nevraToGroupId ("hasEpoch", Nothing, "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, no epoch, is specified" $
        withDb (nevraToGroupId ("noEpoch", Just "7", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, has wrong epoch" $
        withDb (nevraToGroupId ("hasEpoch", Just "8", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong name" $
        withDb (nevraToGroupId ("missingEpoch", Just "7", "1.0", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong version" $
        withDb (nevraToGroupId ("hasEpoch", Just "7", "1.1", "1.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong release" $
        withDb (nevraToGroupId ("hasEpoch", Just "7", "1.0", "2.el7", "x86_64")) >>= (`shouldBe` Nothing)

    it "nevraToGroupId, wrong arch" $
        withDb (nevraToGroupId ("hasEpoch", Just "7", "1.0", "1.el7", "i686")) >>= (`shouldBe` Nothing)
