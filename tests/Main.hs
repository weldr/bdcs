module Main(main) where

import           Test.Tasty(defaultMain, testGroup)
import qualified RPM.Version.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ RPM.Version.Tests.vercmpTests ]
