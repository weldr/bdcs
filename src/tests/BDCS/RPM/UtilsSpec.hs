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

module BDCS.RPM.UtilsSpec(spec)
 where

import Test.Hspec

import BDCS.RPM.Utils(splitFilename)

spec :: Spec
spec = describe "BDCS.RPM.Utils Tests" $ do
    it "N-E:V-R.A.rpm" $
        splitFilename "libwhatever-7:1.0-1.el7.x86_64.rpm" `shouldBe` ("libwhatever", Just "7", "1.0", "1.el7", "x86_64")

    it "no .rpm" $
        splitFilename "libwhatever-7:1.0-1.el7.x86_64" `shouldBe` ("libwhatever", Just "7", "1.0", "1.el7", "x86_64")

    it "no epoch" $
        splitFilename "libwhatever-1.0-1.el7.x86_64.rpm" `shouldBe` ("libwhatever", Nothing, "1.0", "1.el7", "x86_64")

    it "no epoch or .rpm" $
        splitFilename "libwhatever-1.0-1.el7.x86_64" `shouldBe` ("libwhatever", Nothing, "1.0", "1.el7", "x86_64")

    it "hyphen in name, no epoch" $
        splitFilename "libwhatever-ng-1.0-1.el7.x86_64" `shouldBe` ("libwhatever-ng", Nothing, "1.0", "1.el7", "x86_64")

    it "hyphen in name with epoch" $
        splitFilename "libwhatever-ng-7:1.0-1.el7.x86_64" `shouldBe` ("libwhatever-ng", Just "7", "1.0", "1.el7", "x86_64")

    it "numbers in name, no epoch" $
        splitFilename "libwhatever-ng2-1.0-1.el7.x86_64" `shouldBe` ("libwhatever-ng2", Nothing, "1.0", "1.el7", "x86_64")

    it "numbers in name with epoch" $
        splitFilename "libwhatever-ng2-7:1.0-1.el7.x86_64" `shouldBe` ("libwhatever-ng2", Just "7", "1.0", "1.el7", "x86_64")
