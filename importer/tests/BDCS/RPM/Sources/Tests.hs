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

module BDCS.RPM.Sources.Tests(tests)
 where

import Control.Exception(evaluate)
import Test.Tasty(TestTree, testGroup)
import Test.Tasty.HUnit((@=?), testCase)

import BDCS.DB(Projects(..))
import BDCS.Exceptions(DBException(..))
import BDCS.RPM.Sources(mkSource)
import RPM.Tags(Tag(..))

import Utils(assertException, fakeKey)

raiseTests :: TestTree
raiseTests = testGroup "Raise Exceptions"
    [ testCase "No License raises" $
               assertException (MissingRPMTag "License")
                               (evaluate $ mkSource [ Version "" ]
                                                    fakeKey),
      testCase "No Version raises" $
               assertException (MissingRPMTag "Version")
                               (evaluate $ mkSource [ License "" ]
                                                    fakeKey)
    ]

tests :: TestTree
tests = testGroup "BDCS.RPM.Sources Tests"
    [ raiseTests ]
