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

module BDCS.RPM.ProjectsSpec(spec)
 where

import Codec.RPM.Tags(Tag(..))
import Control.Exception(evaluate)
import Test.Hspec

import BDCS.Exceptions(DBException(..))
import BDCS.RPM.Projects(mkProject)

spec :: Spec
spec = describe "BDCS.RPM.Projects Tests" $ do
    it "No SourceRPM raises" $
        evaluate (mkProject [ Summary "", Description "" ]) `shouldThrow` (== MissingRPMTag "SourceRPM")

    it "No Summary raises" $
        evaluate (mkProject [ SourceRPM "a-1-1.src.rpm", Description "" ]) `shouldThrow` (== MissingRPMTag "Summary")

    it "No Description raises" $
        evaluate (mkProject [ SourceRPM "a-1-1.src.rpm", Summary "" ]) `shouldThrow` (== MissingRPMTag "Description")

    -- FIXME:  TESTS TO ADD:
    -- * srpmToName in BDCS/RPM/Projects.hs assumes the string it's given is properly formatted
