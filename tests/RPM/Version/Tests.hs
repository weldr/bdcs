module RPM.Version.Tests(vercmpTests)
 where

import Test.Tasty(TestTree, testGroup)
import Test.Tasty.HUnit(assertEqual, testCase)

import RPM.Version(vercmp)

vercmpTests :: TestTree
vercmpTests = testGroup "vercmp tests" $ map verTestCase versionCases
 where
    verTestCase :: (String, String, Ordering) -> TestTree
    verTestCase (verA, verB, ord) = testCase (verA ++ " " ++ show ord ++ " " ++ verB) $
        assertEqual "" ord (vercmp verA verB)

    versionCases :: [(String, String, Ordering)]
    versionCases = [ ("1.0", "1.0", EQ),
                     ("1.0", "2.0", LT),
                     ("2.0", "1.0", GT),

                     ("2.0.1", "2.0.1", EQ),
                     ("2.0", "2.0.1", LT),
                     ("2.0.1", "2.0", GT),

                     ("2.0.1a", "2.0.1a", EQ),
                     ("2.0.1a", "2.0.1", GT),
                     ("2.0.1", "2.0.1a", LT),

                     ("5.5p1", "5.5p1", EQ),
                     ("5.5p1", "5.5p2", LT),
                     ("5.5p2", "5.5p1", GT),

                     ("5.5p10", "5.5p10", EQ),
                     ("5.5p1", "5.5p10", LT),
                     ("5.5p10", "5.5p1", GT),

                     ("10xyz", "10.1xyz", LT),
                     ("10.1xyz", "10xyz", GT),

                     ("xyz10", "xyz10", EQ),
                     ("xyz10", "xyz10.1", LT),
                     ("xyz10.1", "xyz10", GT),

                     ("xyz.4", "xyz.4", EQ),
                     ("xyz.4", "8", LT),
                     ("8", "xyz.4", GT),
                     ("xyz.4", "2", LT),
                     ("2", "xyz.4", GT),

                     ("5.5p2", "5.6p1", LT),
                     ("5.6p1", "5.5p2", GT),

                     ("5.6p1", "6.5p1", LT),
                     ("6.5p1", "5.6p1", GT),

                     ("6.0.rc1", "6.0", GT),
                     ("6.0", "6.0.rc1", LT),

                     ("10b2", "10a1", GT),
                     ("10a2", "10b2", LT),
                     ("1.0aa", "1.0aa", EQ),
                     ("1.0a", "1.0aa", LT),
                     ("1.0aa", "1.0a", GT),

                     ("10.0001", "10.0001", EQ),
                     ("10.0001", "10.1", EQ),
                     ("10.1", "10.0001", EQ),
                     ("10.0001", "10.0039", LT),
                     ("10.0039", "10.0001", GT),

                     ("4.999.9", "5.0", LT),
                     ("5.0", "4.999.9", GT),

                     ("20101121", "20101121", EQ),
                     ("20101121", "20101122", LT),
                     ("20101122", "20101121", GT),

                     ("2_0", "2_0", EQ),
                     ("2.0", "2_0", EQ),
                     ("2_0", "2.0", EQ),

                     ("a", "a", EQ),
                     ("a+", "a+", EQ),
                     ("a+", "a_", EQ),
                     ("a_", "a+", EQ),
                     ("+a", "+a", EQ),
                     ("+a", "_a", EQ),
                     ("_a", "+a", EQ),
                     ("+_", "+_", EQ),
                     ("_+", "+_", EQ),
                     ("_+", "_+", EQ),
                     ("+", "_", EQ),
                     ("_", "+", EQ),

                     ("1.0~rc1", "1.0~rc1", EQ),
                     ("1.0~rc1", "1.0", LT),
                     ("1.0", "1.0~rc1", GT),
                     ("1.0~rc1", "1.0~rc2", LT),
                     ("1.0~rc2", "1.0~rc1", GT),
                     ("1.0~rc1~git123", "1.0~rc1~git123", EQ),
                     ("1.0~rc1~git123", "1.0~rc1", LT),
                     ("1.0~rc1", "1.0~rc1~git123", GT)
                   ]
