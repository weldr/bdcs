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
module Main where

import Test.Tasty(defaultMain, testGroup)

import qualified BDCS.RPM.Builds.Tests
import qualified BDCS.RPM.Projects.Tests
import qualified BDCS.RPM.Signatures.Tests
import qualified BDCS.RPM.Sources.Tests

main :: IO ()
main = defaultMain $ testGroup "BDCS.RPM Tests" [
    BDCS.RPM.Builds.Tests.tests,
    BDCS.RPM.Projects.Tests.tests,
    BDCS.RPM.Signatures.Tests.tests,
    BDCS.RPM.Sources.Tests.tests
 ]
