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
{-# LANGUAGE OverloadedStrings #-}

module BDCS.RPM.Projects.Tests(tests)
 where

import Control.Exception(evaluate)
import Test.Tasty(TestTree, testGroup)
import Test.Tasty.HUnit((@=?), testCase)

import BDCS.DB(Projects(..))
import BDCS.Exceptions(DBException(..))
import BDCS.RPM.Projects(mkProject)
import RPM.Tags(Tag(..))

import Utils(assertException)

raiseTests :: TestTree
raiseTests = testGroup "Raise Exceptions"
    [ testCase "No SourceRPM raises" $
               assertException (MissingRPMTag "SourceRPM")
                               (evaluate $ mkProject [ Summary "", Description "" ]),
      testCase "No Summary raises" $
               assertException (MissingRPMTag "Summary")
                               (evaluate $ mkProject [ SourceRPM "a-1-1.src.rpm", Description "" ]),
      testCase "No Description raises" $
               assertException (MissingRPMTag "Description")
                               (evaluate $ mkProject [ SourceRPM "a-1-1.src.rpm", Summary "" ])
    ]

tests :: TestTree
tests = testGroup "BDCS.RPM.Projects Tests"
    [ raiseTests ]
    -- FIXME:  TESTS TO ADD:
    -- * srpmToName in BDCS/RPM/Projects.hs assumes the string it's given is properly formatted
