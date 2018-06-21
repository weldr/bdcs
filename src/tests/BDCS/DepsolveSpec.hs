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

module BDCS.DepsolveSpec(spec)
 where

import           Control.Monad.Except(runExceptT)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.Hspec

import BDCS.Depsolve

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
    describe "BDCS.Depsolve.formulaToCNF tests" $ do
        it "atom" $
            formulaToCNF (Atom '1') `shouldBe` [[CNFAtom (CNFOriginal '1')]]

        it "not" $
            formulaToCNF (Not '1') `shouldBe` [[CNFNot (CNFOriginal '1')]]

        it "and" $
            formulaToCNF (And [Atom '1', Atom '2', Atom '3', Atom '4']) `shouldBe` [[CNFAtom (CNFOriginal '1')],
                                                                                    [CNFAtom (CNFOriginal '2')],
                                                                                    [CNFAtom (CNFOriginal '3')],
                                                                                    [CNFAtom (CNFOriginal '4')]]

        it "Or 0 items" $
            formulaToCNF (Or [] :: Formula ()) `shouldBe` ([[]] :: CNFFormula ())

        it "Or 1 item" $
            formulaToCNF (Or [Atom '1']) `shouldBe` [[CNFAtom (CNFOriginal '1')]]

        -- See the comment in formulaToCNF for more detail. In short, for 1 OR 2 OR 3:
        --    start with (1) OR (2 OR 3) ==> (sub 0 -> 1) AND (NOT(sub 0) -> (2 OR 3))
        --    the left half becomes (NOT(sub0) OR 1), which is the first part of the result
        --    the right half becomes (sub0 OR (2 OR 3))
        --    recurse on (2 OR 3), adding sub0 to the front of each result

        it "Or 2 items" $
            formulaToCNF (Or [Atom '1', Atom '2']) `shouldBe` [[CNFNot  (CNFSubstitute 0),
                                                                CNFAtom (CNFOriginal '1')],
                                                               [CNFAtom (CNFSubstitute 0),
                                                                CNFAtom (CNFOriginal '2')]]
        it "Or 3 items" $
            formulaToCNF (Or [Atom '1', Atom '2', Atom '3']) `shouldBe` [[CNFNot  (CNFSubstitute 0),
                                                                          CNFAtom (CNFOriginal '1')],
                                                                         [CNFAtom (CNFSubstitute 0),
                                                                          CNFNot  (CNFSubstitute 1),
                                                                          CNFAtom (CNFOriginal '2')],
                                                                         [CNFAtom (CNFSubstitute 0),
                                                                          CNFAtom (CNFSubstitute 1),
                                                                          CNFAtom (CNFOriginal '3')]]

    describe "BDCS.Depsolve.pureLiteralEliminate tests" $ do
        it "pureLiteralEliminate, empty list" $
            pureLiteralEliminate Set.empty Map.empty ([] :: CNFFormula Char) `shouldBe` Map.empty

        it "already assigned, matches" $ do
            let assignments = Map.singleton (CNFOriginal '1') True
            let formula = [[CNFAtom (CNFOriginal '1')]]
            let solution = assignments
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "already assigned, does not match" $ do
            let assignments = Map.singleton (CNFOriginal '1') True
            let formula = [[CNFNot (CNFOriginal '1')]]
            let solution = Map.empty
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "pureLiteralEliminate, pure, appears once" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')]]
            let solution = Map.singleton (CNFOriginal '1') True
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "pure, appears once negative" $ do
            let assignments = Map.empty
            let formula = [[CNFNot (CNFOriginal '1')]]
            let solution = Map.singleton (CNFOriginal '1') False
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "pure, appears multiple times" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFAtom (CNFOriginal '1'),
                            CNFAtom (CNFOriginal '2')],
                           [CNFNot  (CNFOriginal '2')]]
            let solution = Map.singleton (CNFOriginal '1') True
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "pure, appears multiple times negative" $ do
            let assignments = Map.empty
            let formula = [[CNFNot  (CNFOriginal '1')],
                           [CNFNot  (CNFOriginal '1'),
                            CNFAtom (CNFOriginal '2')],
                           [CNFNot  (CNFOriginal '2')]]
            let solution = Map.singleton (CNFOriginal '1') False
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "unpure" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFNot  (CNFOriginal '1')]]
            let solution = Map.empty
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

        it "unpure 2" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1'),
                            CNFNot  (CNFOriginal '1')],
                           [CNFNot  (CNFOriginal '1')]]
            let solution = Map.empty
            pureLiteralEliminate Set.empty assignments formula `shouldBe` solution

    describe "BDCS.Depsolve.unitPropagate tests" $ do
        it "empty list" $
            runExceptT (unitPropagate Map.empty ([] :: CNFFormula Char)) >>=
                (`shouldBe` Right (Map.empty, []))

        it "one thing" $
            runExceptT (unitPropagate Map.empty [[CNFAtom (CNFOriginal '1')]]) >>=
                (`shouldBe` Right (Map.singleton (CNFOriginal '1') True, []))

        it "unit then elimate clause" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFAtom (CNFOriginal '1'),
                            CNFNot  (CNFOriginal '2')]]
            let assignments' = Map.singleton (CNFOriginal '1') True
            let formula' = []
            runExceptT (unitPropagate assignments formula) >>= (`shouldBe` Right (assignments', formula'))

        it "unit then elimate impossible" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFAtom (CNFOriginal '2'),
                            CNFNot  (CNFOriginal '1')]]
            let assignments' = Map.singleton (CNFOriginal '1') True
            let formula' = [[CNFAtom (CNFOriginal '2')]]
            runExceptT (unitPropagate assignments formula) >>= (`shouldBe` Right (assignments', formula'))

        it "unit then repeated" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFAtom (CNFOriginal '1')]]
            let assignments' = Map.singleton (CNFOriginal '1') True
            let formula' = []
            runExceptT (unitPropagate assignments formula) >>= (`shouldBe` Right (assignments', formula'))

        it "unit then unsolvable unit" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFNot  (CNFOriginal '1')]]
            runExceptT (unitPropagate assignments formula) >>= (`shouldBe` Left "Unable to solve expression")

        it "units then unsolvable clause" $ do
            let assignments = Map.empty
            let formula = [[CNFAtom (CNFOriginal '1')],
                           [CNFAtom (CNFOriginal '2')],
                           [CNFNot  (CNFOriginal '1'),
                            CNFNot  (CNFOriginal '2')]]
            runExceptT (unitPropagate assignments formula) >>= (`shouldBe` Left "Unable to solve expression")

    describe "BDCS.Depsolve.solveCNF tests" $ do
        it "empty formula" $ do
            let formula = [] :: (CNFFormula Char)
            let solution = [] :: [DepAssignment Char]
            runExceptT (solveCNF formula) >>= (`shouldBe` Right solution)

        it "singleton" $ do
            let formula = [[CNFAtom (CNFOriginal '1')]]
            let solution = [('1', True)]
            runExceptT (solveCNF formula) >>= (`shouldBe` Right solution)
