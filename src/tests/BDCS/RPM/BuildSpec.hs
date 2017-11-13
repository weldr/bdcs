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

module BDCS.RPM.BuildSpec(spec)
 where

import Codec.RPM.Tags(Tag(..))
import Control.Exception(evaluate)
import Test.Hspec

import BDCS.DB(Builds(..))
import BDCS.Exceptions(DBException(..))
import BDCS.RPM.Builds(mkBuild)

import Utils(fakeKey)

spec :: Spec
spec = describe "BDCS.RPM.Builds Tests" $ do
    it "Handles no Epoch" $
        buildsEpoch (mkBuild [ Release "1", Arch "x86-64", BuildTime 0, ChangeLogText [""] ] fakeKey) `shouldBe` 0

    it "No Release raises" $
        evaluate (mkBuild [ Arch "x86-64", BuildTime 0, ChangeLogText [""] ] fakeKey) `shouldThrow` (== MissingRPMTag "Release")

    it "No Arch raises" $
        evaluate (mkBuild [ Release "1", BuildTime 0, ChangeLogText [""] ] fakeKey) `shouldThrow` (== MissingRPMTag "Arch")

    it "No BuildTime raises" $
        evaluate (mkBuild [ Release "1", Arch "x86-64", ChangeLogText [""] ] fakeKey) `shouldThrow` (== MissingRPMTag "BuildTime")

    it "No ChangeLogText raises" $
        evaluate (mkBuild [ Release "1", Arch "x86-64", BuildTime 0 ] fakeKey) `shouldThrow` (== MissingRPMTag "ChangeLogText")
