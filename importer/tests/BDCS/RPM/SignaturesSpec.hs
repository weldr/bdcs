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

module BDCS.RPM.SignaturesSpec(spec)
 where

import Codec.RPM.Tags(Tag(..))
import Control.Exception(evaluate)
import Test.Hspec

import BDCS.DB(Projects(..))
import BDCS.Exceptions(DBException(..))
import BDCS.RPM.Signatures(mkRSASignature, mkSHASignature)

import Utils(fakeKey)

spec :: Spec
spec = describe "BDCS.RPM.Signatures Tests" $ do
    it "No RSAHeader raises" $
        evaluate (mkRSASignature [ ] fakeKey) `shouldThrow` (== MissingRPMTag "RSAHeader")

    it "No SHA1Header raises" $
        evaluate (mkSHASignature [ ] fakeKey) `shouldThrow` (== MissingRPMTag "SHA1Header")
