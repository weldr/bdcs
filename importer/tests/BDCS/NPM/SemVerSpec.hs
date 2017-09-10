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

module BDCS.NPM.SemVerSpec(spec)
 where

import           Control.Monad(forM_)
import           Data.Either(isLeft)
import           Data.List(sort)
import qualified Data.Text as T
import           Test.Hspec
import           Test.HUnit(assertBool)

import BDCS.NPM.SemVer(SemVer(..), SemVerIdentifier(..), parseSemVer, parseSemVerRangeSet, satisfies, toText)

spec :: Spec
spec = describe "BDCS.NPM.SemVer Tests" $ do
    -- tests adapted from node-semver
    it "comparison tests" $ forM_ comparisonData $ \(v1, v2) -> do
        testOrd v1 v2 GT
        testOrd v2 v1 LT
        testOrd v1 v1 EQ
        testOrd v2 v2 EQ

    it "equality tests" $ forM_ equalityData $ \(v1, v2) -> do
        testOrd v1 v2 EQ
        testOrd v1 v1 EQ
        testOrd v2 v2 EQ

    it "range tests" $ forM_ rangeData $ \(version, range) ->
        testSatisfies version range True

    it "negative range tests" $ forM_ negativeRangeData $ \(version, range) ->
        testSatisfies version range False

    -- cases from various other npm tests that involve parse errors
    it "range parse errors" $ do
        parseSemVerRangeSet "blerg" `shouldSatisfy` isLeft
        parseSemVerRangeSet "git+https://user:password0123@github.com/foo" `shouldSatisfy` isLeft

    it "version parse errors" $ do
        parseSemVer "1.2.3.4" `shouldSatisfy` isLeft
        parseSemVer "NOT VALID" `shouldSatisfy` isLeft
        parseSemVer "1.2" `shouldSatisfy` isLeft
        parseSemVer "" `shouldSatisfy` isLeft
        parseSemVer "not.a.version" `shouldSatisfy` isLeft

    it "loose version parse" $ do
        parseSemVer "01.02.03" `shouldBe` Right (SemVer 1 2 3 [] [])
        parseSemVer "1.2.3-beta.01" `shouldBe` Right (SemVer 1 2 3 [TextIdentifier "beta", NumericIdentifier 1] [])
        parseSemVer "   =1.2.3" `shouldBe` Right (SemVer 1 2 3 [] [])
        parseSemVer "1.2.3foo" `shouldBe` Right (SemVer 1 2 3 [TextIdentifier "foo"] [])
        parseSemVer "1.2.3" `shouldBe` Right (SemVer 1 2 3 [] [])
        parseSemVer " 1.2.3 " `shouldBe` Right (SemVer 1 2 3 [] [])
        parseSemVer " 3.2.3-pre" `shouldBe` Right (SemVer 3 2 3 [TextIdentifier "pre"] [])
        parseSemVer "v5.2.3" `shouldBe` Right (SemVer 5 2 3 [] [])
        parseSemVer " v8.2.3 " `shouldBe` Right (SemVer 8 2 3 [] [])
        parseSemVer "\t13.2.3" `shouldBe` Right (SemVer 13 2 3 [] [])
        parseSemVer "=21.2.3" `shouldBe` Right (SemVer 21 2 3 [] [])
        parseSemVer "v=34.2.3" `shouldBe` Right (SemVer 34 2 3 [] [])

    it "sort test" $
        sort [SemVer 1 2 3 [] [], SemVer 5 9 6 [] [], SemVer 0 1 2 [] []] `shouldBe`
             [SemVer 0 1 2 [] [], SemVer 1 2 3 [] [], SemVer 5 9 6 [] []]

    -- some extra tests not from semver
    it "buildmeta tests" $ do
        let v1 = SemVer 1 2 3 [] [TextIdentifier "build", NumericIdentifier 1]
        let v2 = SemVer 1 2 3 [] [TextIdentifier "build", NumericIdentifier 2]
        let v3 = SemVer 1 2 3 [] []

        v1 `shouldNotBe` v2
        v1 `shouldNotBe` v3
        v2 `shouldNotBe` v1
        v2 `shouldNotBe` v3
        v3 `shouldNotBe` v1
        v3 `shouldNotBe` v2

        compare v1 v2 `shouldBe` EQ
        compare v1 v3 `shouldBe` EQ
        compare v2 v1 `shouldBe` EQ
        compare v2 v3 `shouldBe` EQ
        compare v3 v1 `shouldBe` EQ
        compare v3 v2 `shouldBe` EQ

    it "wildcard versions" $
        parseSemVer "1.2.*" `shouldSatisfy` isLeft

    it "corner cases" $ do
        testSatisfies "1.3.0" "<= 1.2.*" False
        testSatisfies "1.2.9" "<= 1.2.*" True
        testSatisfies "1.2.0" "<= 1.2.*" True
        testSatisfies "1.2.0" "<  1.2.*" False
        testSatisfies "1.1.9" "<  1.2.*" True
        testSatisfies "1.1.0" "<  1.2.*" True
        testSatisfies "1.3.0" ">= 1.2.*" True
        testSatisfies "1.2.0" ">= 1.2.*" True
        testSatisfies "1.1.9" ">= 1.2.*" False
        testSatisfies "1.2.0" ">  1.2.*" False
        testSatisfies "1.2.9" ">  1.2.*" False
        testSatisfies "1.3.0" ">  1.2.*" True

    it "toText tests" $ do
        toText (SemVer 1 2 3 [] []) `shouldBe` "1.2.3"
        toText (SemVer 0 0 0 [] []) `shouldBe` "0.0.0"
        toText (SemVer 1 2 3 [TextIdentifier "alpha"] []) `shouldBe` "1.2.3-alpha"
        toText (SemVer 1 2 3 [NumericIdentifier 47] []) `shouldBe` "1.2.3-47"
        toText (SemVer 1 2 3 [TextIdentifier "alpha", NumericIdentifier 47] []) `shouldBe` "1.2.3-alpha.47"
        toText (SemVer 1 2 3 [] [TextIdentifier "asdf"]) `shouldBe` "1.2.3+asdf"
        toText (SemVer 1 2 3 [] [NumericIdentifier 12]) `shouldBe` "1.2.3+12"
        toText (SemVer 1 2 3 [] [NumericIdentifier 12, TextIdentifier "asdf"]) `shouldBe` "1.2.3+12.asdf"
        toText (SemVer 1 2 3 [TextIdentifier "alpha", NumericIdentifier 47] [NumericIdentifier 12, TextIdentifier "asdf"]) `shouldBe` "1.2.3-alpha.47+12.asdf"
 where
    testSatisfies :: T.Text -> T.Text -> Bool -> Expectation
    testSatisfies ver range result = let
        semver = parseSemVer ver
        semverRange = parseSemVerRangeSet range
     in
        case (semver, semverRange) of
            (Left e, _)        -> expectationFailure $ "Error parsing semver " ++ T.unpack ver ++ ": " ++ show e
            (_, Left e)        -> expectationFailure $ "Error parsing semver range " ++ T.unpack range ++ ": " ++ show e
            (Right s, Right r) -> assertBool ("satisfy test failed: " ++ show ver ++ " `satisfies` " ++ show range ++ " == " ++ show result)
                                             (s `satisfies` r == result)

    testOrd :: T.Text -> T.Text -> Ordering -> Expectation
    testOrd v1 v2 result = let
        semver1 = parseSemVer v1
        semver2 = parseSemVer v2
     in
        case (semver1, semver2) of
            (Left e, _)          -> expectationFailure $ "Error parsing semver " ++ T.unpack v1 ++ ": " ++ show e
            (_, Left e)          -> expectationFailure $ "Error parsing semver " ++ T.unpack v2 ++ ": " ++ show e
            (Right s1, Right s2) -> assertBool ("ord test failed: " ++ show v1 ++ " " ++ show result ++ " " ++ show v2)
                                               (compare s1 s2 == result)

    -- v1 > v2
    comparisonData :: [(T.Text, T.Text)]
    comparisonData = [
                ("0.0.0",              "0.0.0-foo"),
                ("0.0.1",              "0.0.0"),
                ("1.0.0",              "0.9.9"),
                ("0.10.0",             "0.9.0"),
                ("0.99.0",             "0.10.0"),
                ("2.0.0",              "1.2.3"),
                ("v0.0.0",             "0.0.0-foo"),
                ("v0.0.1",             "0.0.0"),
                ("v1.0.0",             "0.9.9"),
                ("v0.10.0",            "0.9.0"),
                ("v0.99.0",            "0.10.0"),
                ("v2.0.0",             "1.2.3"),
                ("0.0.0",              "v0.0.0-foo"),
                ("0.0.1",              "v0.0.0"),
                ("1.0.0",              "v0.9.9"),
                ("0.10.0",             "v0.9.0"),
                ("0.99.0",             "v0.10.0"),
                ("2.0.0",              "v1.2.3"),
                ("1.2.3",              "1.2.3-asdf"),
                ("1.2.3",              "1.2.3-4"),
                ("1.2.3",              "1.2.3-4-foo"),
                ("1.2.3-5-foo",        "1.2.3-5"),
                ("1.2.3-5",            "1.2.3-4"),
                ("1.2.3-5-foo",        "1.2.3-5-Foo"),
                ("3.0.0",              "2.7.2+asdf"),
                ("1.2.3-a.10",         "1.2.3-a.5"),
                ("1.2.3-a.b",          "1.2.3-a.5"),
                ("1.2.3-a.b",          "1.2.3-a"),
                ("1.2.3-a.b.c.10.d.5", "1.2.3-a.b.c.5.d.100"),
                ("1.2.3-r2",           "1.2.3-r100"),
                ("1.2.3-r100",         "1.2.3-R2")
               ]

    -- v1 == v2
    equalityData :: [(T.Text, T.Text)]
    equalityData = [
                ("1.2.3",            "v1.2.3"),
                ("1.2.3",            "=1.2.3"),
                ("1.2.3",            "v 1.2.3"),
                ("1.2.3",            "= 1.2.3"),
                ("1.2.3",            " v1.2.3"),
                ("1.2.3",            " =1.2.3"),
                ("1.2.3",            " v 1.2.3"),
                ("1.2.3",            " = 1.2.3"),
                ("1.2.3-0",          "v1.2.3-0"),
                ("1.2.3-0",          "=1.2.3-0"),
                ("1.2.3-0",          "v 1.2.3-0"),
                ("1.2.3-0",          "= 1.2.3-0"),
                ("1.2.3-0",          " v1.2.3-0"),
                ("1.2.3-0",          " =1.2.3-0"),
                ("1.2.3-0",          " v 1.2.3-0"),
                ("1.2.3-0",          " = 1.2.3-0"),
                ("1.2.3-1",          "v1.2.3-1"),
                ("1.2.3-1",          "=1.2.3-1"),
                ("1.2.3-1",          "v 1.2.3-1"),
                ("1.2.3-1",          "= 1.2.3-1"),
                ("1.2.3-1",          " v1.2.3-1"),
                ("1.2.3-1",          " =1.2.3-1"),
                ("1.2.3-1",          " v 1.2.3-1"),
                ("1.2.3-1",          " = 1.2.3-1"),
                ("1.2.3-beta",       "v1.2.3-beta"),
                ("1.2.3-beta",       "=1.2.3-beta"),
                ("1.2.3-beta",       "v 1.2.3-beta"),
                ("1.2.3-beta",       "= 1.2.3-beta"),
                ("1.2.3-beta",       " v1.2.3-beta"),
                ("1.2.3-beta",       " =1.2.3-beta"),
                ("1.2.3-beta",       " v 1.2.3-beta"),
                ("1.2.3-beta",       " = 1.2.3-beta"),
                ("1.2.3-beta+build", " = 1.2.3-beta+otherbuild"),
                ("1.2.3+build",      " = 1.2.3+otherbuild"),
                ("1.2.3-beta+build", "1.2.3-beta+otherbuild"),
                ("1.2.3+build",      "1.2.3+otherbuild"),
                ("  v1.2.3+build",   "1.2.3+otherbuild")
            ]

    rangeData :: [(T.Text, T.Text)]
    rangeData = [
                ("1.2.3",       "1.0.0 - 2.0.0"),
                ("1.2.3",       "^1.2.3+build"),
                ("1.3.0",       "^1.2.3+build"),
                ("1.2.3",       "1.2.3-pre+asdf - 2.4.3-pre+asdf"),
                ("1.2.3",       "1.2.3pre+asdf - 2.4.3-pre+asdf"),
                ("1.2.3",       "1.2.3-pre+asdf - 2.4.3pre+asdf"),
                ("1.2.3",       "1.2.3pre+asdf - 2.4.3pre+asdf"),
                ("1.2.3-pre.2", "1.2.3-pre+asdf - 2.4.3-pre+asdf"),
                ("2.4.3-alpha", "1.2.3-pre+asdf - 2.4.3-pre+asdf"),
                ("1.2.3",       "1.2.3+asdf - 2.4.3+asdf"),
                ("1.0.0",       "1.0.0"),
                ("0.2.4",       ">=*"),
                ("1.0.0",       ""),
                ("1.2.3",       "*"),
                ("v1.2.3",       "*"),
                ("1.0.0",       ">=1.0.0"),
                ("1.0.1",       ">=1.0.0"),
                ("1.1.0",       ">=1.0.0"),
                ("1.0.1",       ">1.0.0"),
                ("1.1.0",       ">1.0.0"),
                ("2.0.0",       "<=2.0.0"),
                ("1.9999.9999", "<=2.0.0"),
                ("0.2.9",       "<=2.0.0"),
                ("1.9999.9999", "<2.0.0"),
                ("0.2.9",       "<2.0.0"),
                ("1.0.0",       ">= 1.0.0"),
                ("1.0.1",       ">=  1.0.0"),
                ("1.1.0",       ">=   1.0.0"),
                ("1.0.1",       "> 1.0.0"),
                ("1.1.0",       ">  1.0.0"),
                ("2.0.0",       "<=   2.0.0"),
                ("1.9999.9999", "<= 2.0.0"),
                ("0.2.9",       "<=  2.0.0"),
                ("1.9999.9999", "<    2.0.0"),
                ("0.2.9",       "<\t2.0.0"),
                ("v0.1.97",       ">=0.1.97"),
                ("0.1.97",       ">=0.1.97"),
                ("1.2.4",       "0.1.20 || 1.2.4"),
                ("0.0.0",       ">=0.2.3 || <0.0.1"),
                ("0.2.3",       ">=0.2.3 || <0.0.1"),
                ("0.2.4",       ">=0.2.3 || <0.0.1"),
                ("1.3.4",       "||"),
                ("2.1.3",       "2.x.x"),
                ("1.2.3",       "1.2.x"),
                ("2.1.3",       "1.2.x || 2.x"),
                ("1.2.3",       "1.2.x || 2.x"),
                ("1.2.3",       "x"),
                ("2.1.3",       "2.*.*"),
                ("1.2.3",       "1.2.*"),
                ("2.1.3",       "1.2.* || 2.*"),
                ("1.2.3",       "1.2.* || 2.*"),
                ("1.2.3",       "*"),
                ("2.1.2",       "2"),
                ("2.3.1",       "2.3"),
                ("0.0.9",       "~x"),
                ("2.0.9",       "~2"),
                ("2.4.0",       "~2.4"),
                ("2.4.5",       "~2.4"),
                ("3.2.2",       "~>3.2.1"),
                ("1.2.3",       "~1"),
                ("1.2.3",       "~>1"),
                ("1.2.3",       "~> 1"),
                ("1.0.2",       "~1.0"),
                ("1.0.2",       "~ 1.0"),
                ("1.0.12",       "~ 1.0.3"),
                ("1.0.0",       ">=1"),
                ("1.0.0",       ">= 1"),
                ("1.1.1",       "<1.2"),
                ("1.1.1",       "< 1.2"),
                ("0.5.5",       "~v0.5.4-pre"),
                ("0.5.4",       "~v0.5.4-pre"),
                ("0.7.2",       "=0.7.x"),
                ("0.7.2",       "<=0.7.x"),
                ("0.7.2",       ">=0.7.x"),
                ("0.6.2",       "<=0.7.x"),
                ("1.2.3",       "~1.2.1 >=1.2.3"),
                ("1.2.3",       "~1.2.1 =1.2.3"),
                ("1.2.3",       "~1.2.1 1.2.3"),
                ("1.2.3",       "~1.2.1 >=1.2.3 1.2.3"),
                ("1.2.3",       "~1.2.1 1.2.3 >=1.2.3"),
                ("1.2.3",       "~1.2.1 1.2.3"),
                ("1.2.3",       ">=1.2.1 1.2.3"),
                ("1.2.3",       "1.2.3 >=1.2.1"),
                ("1.2.3",       ">=1.2.3 >=1.2.1"),
                ("1.2.3",       ">=1.2.1 >=1.2.3"),
                ("1.2.8",       ">=1.2"),
                ("1.8.1",       "^1.2.3"),
                ("0.1.2",       "^0.1.2"),
                ("0.1.2",       "^0.1"),
                ("0.0.1",       "^0.0.1"),
                ("1.4.2",       "^1.2"),
                ("1.4.2",       "^1.2 ^1"),
                ("1.2.3-pre",   "^1.2.3-alpha"),
                ("1.2.0-pre",   "^1.2.0-alpha"),
                ("0.0.1-beta",  "^0.0.1-alpha"),
                ("0.1.1-beta",  "^0.1.1-alpha"),
                ("1.2.3",       "^x"),
                ("0.9.7",       "x - 1.0.0"),
                ("0.9.7",       "x - 1.x"),
                ("1.9.7",       "1.0.0 - x"),
                ("1.9.7",       "1.x - x"),
                ("7.9.9",       "<=7.x")
            ]

    negativeRangeData :: [(T.Text, T.Text)]
    negativeRangeData = [
                ("2.2.3",       "1.0.0 - 2.0.0"),
                ("1.2.3-pre.2", "1.2.3+asdf - 2.4.3+asdf"),
                ("2.4.3-alpha", "1.2.3+asdf - 2.4.3+asdf"),
                ("2.0.0",       "^1.2.3+build"),
                ("1.2.0",       "^1.2.3+build"),
                ("1.2.3-pre",   "^1.2.3"),
                ("1.2.0-pre",   "^1.2"),
                ("1.3.0-beta",  ">1.2"),
                ("1.2.3-beta",  "<=1.2.3"),
                ("1.2.3-beta",  "^1.2.3"),
                ("0.7.0-asdf",  "=0.7.x"),
                ("0.7.0-asdf",  ">=0.7.x"),
                ("1.0.0beta",   "1"),
                ("1.0.0beta",   "<1"),
                ("1.0.0beta",   "< 1"),
                ("1.0.1",       "1.0.0"),
                ("0.0.0",       ">=1.0.0"),
                ("0.0.1",       ">=1.0.0"),
                ("0.1.0",       ">=1.0.0"),
                ("0.0.1",       ">1.0.0"),
                ("0.1.0",       ">1.0.0"),
                ("3.0.0",       "<=2.0.0"),
                ("2.9999.9999", "<=2.0.0"),
                ("2.2.9",       "<=2.0.0"),
                ("2.9999.9999", "<2.0.0"),
                ("2.2.9",       "<2.0.0"),
                ("v0.1.93",     ">=0.1.97"),
                ("0.1.93",      ">=0.1.97"),
                ("1.2.3",       "0.1.20 || 1.2.4"),
                ("0.0.3",       ">=0.2.3 || <0.0.1"),
                ("0.2.2",       ">=0.2.3 || <0.0.1"),
                ("1.1.3",       "2.x.x"),
                ("3.1.3",       "2.x.x"),
                ("1.3.3",       "1.2.x"),
                ("3.1.3",       "1.2.x || 2.x"),
                ("1.1.3",       "1.2.x || 2.x"),
                ("1.1.3",       "2.*.*"),
                ("3.1.3",       "2.*.*"),
                ("1.3.3",       "1.2.*"),
                ("3.1.3",       "1.2.* || 2.*"),
                ("1.1.3",       "1.2.* || 2.*"),
                ("1.1.2",       "2"),
                ("2.4.1",       "2.3"),
                ("2.5.0",       "~2.4"),
                ("2.3.9",       "~2.4"),
                ("3.3.2",       "~>3.2.1"),
                ("3.2.0",       "~>3.2.1"),
                ("0.2.3",       "~1"),
                ("2.2.3",       "~>1"),
                ("1.1.0",       "~1.0"),
                ("1.0.0",       "<1"),
                ("1.1.1",       ">=1.2"),
                ("2.0.0beta",   "1"),
                ("0.5.4-alpha", "~v0.5.4-beta"),
                ("0.8.2",       "=0.7.x"),
                ("0.6.2",       ">=0.7.x"),
                ("0.7.2",       "<0.7.x"),
                ("1.2.3-beta",  "<1.2.3"),
                ("1.2.3-beta",  "=1.2.3"),
                ("1.2.8",       ">1.2"),
                ("0.0.2",       "^0.0.1"),
                ("2.0.0-alpha", "^1.2.3"),
                ("1.2.2",       "^1.2.3"),
                ("1.1.9",       "^1.2"),
                ("2.0.0-pre",   "^1.2.3")
            ]
