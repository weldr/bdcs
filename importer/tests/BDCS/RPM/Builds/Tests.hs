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
module BDCS.RPM.Builds.Tests(tests)
 where

import Control.Exception(evaluate)
import Test.Tasty(TestTree, testGroup)
import Test.Tasty.HUnit((@=?), testCase)

import BDCS.DB(Builds(..))
import BDCS.Exceptions(DBException(..))
import BDCS.RPM.Builds(mkBuild)
import RPM.Tags(Tag(..))

import Utils(assertException, fakeKey)

raiseTests :: TestTree
raiseTests = testGroup "Raise Exceptions"
    [ testCase "No Release raises" $
               assertException (MissingRPMTag "Release")
                               (evaluate $ mkBuild [ Arch "x86-64" 0 0, BuildTime 0 0 0, ChangeLogText [""] 0 0 ]
                                                   fakeKey),
      testCase "No Arch raises" $
               assertException (MissingRPMTag "Arch")
                               (evaluate $ mkBuild [ Release "1" 0 0, BuildTime 0 0 0, ChangeLogText [""] 0 0 ]
                                                   fakeKey),
      testCase "No BuildTime raises" $
               assertException (MissingRPMTag "BuildTime")
                               (evaluate $ mkBuild [ Release "1" 0 0, Arch "x86-64" 0 0, ChangeLogText [""] 0 0 ]
                                                   fakeKey),
      testCase "No ChangeLogText raises" $
               assertException (MissingRPMTag "ChangeLogText")
                               (evaluate $ mkBuild [ Release "1" 0 0, Arch "x86-64" 0 0, BuildTime 0 0 0 ]
                                                   fakeKey)
    ]

tests :: TestTree
tests = testGroup "BDCS.RPM.Builds Tests"
    [ testCase "Handles no Epoch" $
               0 @=? buildsEpoch (mkBuild [ Release "1" 0 0, Arch "x86-64" 0 0, BuildTime 0 0 0, ChangeLogText [""] 0 0 ]
                                          fakeKey),
      raiseTests
    ]
