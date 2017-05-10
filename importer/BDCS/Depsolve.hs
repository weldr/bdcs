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

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module BDCS.Depsolve(Formula(..),
                     CNFLiteral(..),
                     CNFAtom(..),
                     CNFFormula,
                     formulaToCnf)
 where

import           Control.Monad.State(State, evalState, state)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(mapMaybe)
import           Data.Monoid((<>))
import           Data.Set(Set)
import qualified Data.Set as Set

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = fmap concat (mapM fn lst)

-- A logical proposition in negation normal form
-- (i.e., NOT is applied only to atoms, not sub-formulas)
data Formula a = Atom a
               | Not a
               | Or [Formula a]
               | And [Formula a]

 deriving(Eq, Show)

-- Conjunctive Normal Form (CNF) is, essentially, and AND of ORs. The formula is of the form
-- (a1 OR a2 ...) AND (b1 OR b2 OR ...) AND ...
-- where each a1, b2, etc is an atom or a not-atom.
-- To keep the size of converted formulas under control, some extra variables are added to represent
-- sub-formulas from the original expression.
data CNFLiteral a = CNFOriginal a
                  | CNFSubstitute Int
 deriving(Eq, Ord, Show)

data CNFAtom a = CNFAtom (CNFLiteral a)
               | CNFNot (CNFLiteral a)
 deriving(Eq, Ord, Show)

type CNFFormula a = [[CNFAtom a]]

formulaToCnf :: Formula a -> CNFFormula a
formulaToCnf f =
    -- wrap the call in a State Int starting at 0 to create a counter for substitution variables
    evalState (formulaToCnf' f) 0
 where
    formulaToCnf' :: Formula a -> State Int (CNFFormula a)

    -- easy ones: a becomes AND(OR(a)), NOT(a) becomes AND(OR(NOT(a)))
    formulaToCnf' (Atom x) = return [[CNFAtom (CNFOriginal x)]]
    formulaToCnf' (Not x)  = return [[CNFNot (CNFOriginal x)]]

    -- -- for an expression of the form And [a1, a2, a3, ...], we need to convert
    -- each a1, a2, ... to CNF and concatenate the results.
    --
    -- In other words, for And [a1, a2], map the list to something like
    --
    -- [And[Or[a1_or1_1, a1_or1_2],
    --      Or[a1_or2_1, a1_or2_2]],
    --  And[Or[a2_or1, a2_or1_2],
    --      Or[a2_or2_1, a2_or2_2]]]
    --
    -- which is equivalent to
    --
    -- And[Or[a1_or1_1, a1_or1_2],
    --     Or[a1_or2_1, a1_or2_2],
    --     Or[a2_or1_1, a2_or1_2],
    --     Or[a2_or2_1, a2_or2_2]]
    formulaToCnf' (And andFormulas) = concatMapM formulaToCnf' andFormulas

    -- For Or, the equivalent formula is exponentially larger than the original, so instead
    -- create an equisatisfiable formula using new substitution variables, via Tseytin transformations.
    --
    -- For a given expression:
    --
    --   a1 OR a2 OR a3 ...
    --
    -- we start out by creating an equisatisfiable expression with new variables:
    --
    --   (Z1 -> a1) AND (NOT(Z1) -> (a2 OR a3 ...))
    --
    -- starting with the left side of the AND, the expression is equivalent to
    --
    --   (NOT(Z1) OR a1)
    --
    -- and if we can convert a1 into CNF, we get an expression of the form
    --
    --   NOT(Z1) OR (a1_1 AND a1_2 AND ...)
    --
    -- where each a1_1, a1_2 etc is an OR. We can then use the distributive property to create
    --
    --   (NOT(Z1) OR a1_1) AND (NOT(Z1) OR a1_2) AND ...
    --
    -- which is CNF. Then, for the right hand side of that original AND pair up there, we're
    -- left with:
    --
    --   Z1 OR (a2 OR a3 OR ...)
    --
    -- so to recurse, we convert (a2 OR a3 OR ...) to CNF, and then convert (Z1 OR (CNF))
    -- to CNF via distribution as above. We then have <cnf-of-head> AND <cnf-of-tail>, which is CNF.

    -- end of recursion: OR of nothing is nothing, OR of 1 thing is just that thing
    formulaToCnf' (Or []) = return [[]]
    formulaToCnf' (Or [x]) = formulaToCnf' x

    formulaToCnf' (Or (x:xs)) = do
        -- Get and increment the counter
        subVar <- state $ \i -> (CNFSubstitute i, i+1)

        -- recurse on the left hand expression
        lhCNF <- formulaToCnf' x

        -- distribute NOT(subVar) AND lhCNF by adding NOT(subVar) into each of the OR lists
        let lhSubCNF = map (CNFNot subVar:) lhCNF

        -- recurse on the right hand side
        rhCNF <- formulaToCnf' (Or xs)

        -- distribute subVar across the right hand expression
        let rhSubCNF = map (CNFAtom subVar:) rhCNF

        -- combine the results
        return (lhSubCNF ++ rhSubCNF)

-- assignments to literals that will satisfy a formula
type DepAssignment a = (a, Bool)

-- internal type for the variable=bool assignments
type AssignmentMap a = Map (CNFLiteral a) Bool

-- if the formula is unsolvable, returns Nothing, other Just the list of assignments
-- This function uses the Davis-Putnam-Logemann-Loveman procedure for satisfying the formula, which is as follows:
--   Repeatedly simplify the formula using unit propagation and pure literal elimination:
--     unit propagation looks for a clause that contains only one literal, assigns it, and then removes clauses satisfied by the assignment
--       for example, in
--         (a OR b OR c) AND (a) AND (b OR ~c)
--
--       (a) appears alone, so it must be true. We can then remove both (a) and (a OR b OR c), as these clauses are now satisfied
--       by a=True.
--
--     pure literal elimation looks for literals that only appear as true or false. In the above example, b is only present
--     in the formula as True (there is no ~b in the formula), so we can assign b=True and then remove all clauses containing b.
--
--  once simplified, pick a literal and assign it to True and try to satisfy the formula. If that doesn't work, assign to to False.
--
--  Repeat until solved.
solveCNF :: Ord a => CNFFormula a -> Maybe [DepAssignment a]
solveCNF formula = solveCNF' Map.empty formula
 where
    -- helper function that takes an assignment map and a formula
    solveCNF' :: Ord a => AssignmentMap a -> CNFFormula a -> Maybe [DepAssignment a]
    solveCNF' assignments formula =
        -- simplify the formula. simplify will recurse as necessary
        case simplify assignments formula of
            -- if things failed during simplify, the formula is unsatisfiable
            Nothing -> Nothing
            -- otherwise, try an assignment and recurse
            Just (assignments', formula') -> solveSimplifiedCNF assignments' formula'

    solveSimplifiedCNF :: Ord a => AssignmentMap a -> CNFFormula a -> Maybe [DepAssignment a]
    -- If the formula is empty, it's satisfied. Return the assignments map as a list
    solveSimplifiedCNF assignments [] = Just $ assignmentsToList assignments

    -- Otherwise, try assigning the first literal and recurse
    -- If True doesn't work, try False. If that doesn't work, the formula is unsovlable
    solveSimplifiedCNF assignments f@((firstLiteral:_):_) = try True <> try False
     where
        try val = solveCNF' (assignAtom assignments firstLiteral val) f

    -- This case shouldn't come out of simplify, but include it to complete the pattern match
    solveSimplifiedCNF _ [[]] = Nothing

    -- Insert atom=val into the assignments map
    assignAtom :: Ord a => AssignmentMap a -> CNFAtom a -> Bool -> AssignmentMap a
    assignAtom assignments atom val = Map.insert (atomToLiteral atom) val assignments

    -- unwrap an atom
    atomToLiteral :: CNFAtom a -> CNFLiteral a
    atomToLiteral (CNFAtom x) = x
    atomToLiteral (CNFNot x)  = x

    atomToBool :: CNFAtom a -> Bool
    atomToBool (CNFAtom _) = True
    atomToBool (CNFNot _)  = False

    -- unwrap original values, discard substitutes
    literalToOriginal :: CNFLiteral a -> Maybe a
    literalToOriginal (CNFOriginal x) = Just x
    literalToOriginal _ = Nothing

    assignmentsToList :: AssignmentMap a -> [DepAssignment a]
    assignmentsToList assignments = let
        -- start by getting everything out of the map into a list of (CNFLiteral, Bool)
        literalList = Map.foldlWithKey (\acc key val -> (key, val):acc) [] assignments
     in
        -- map each (literal, bool) to Maybe (orig, bool), mapMaybe will filter out the Nothings
        mapMaybe (\(literal, value) -> (,value) <$> literalToOriginal literal) literalList

    simplify :: Ord a => AssignmentMap a -> CNFFormula a -> Maybe (AssignmentMap a, CNFFormula a)
    simplify assignments formula = let
        -- ple is written such that it only changes assignments, not formula, and the new assignments get removed by unit propagation
        -- which is to say, do pure literal elimination first, then unit propagation
        pleAssignments = pureLiteralEliminate assignments Set.empty formula
     in
        case unitPropagate pleAssignments formula of
            Nothing -> Nothing
            result@(Just (upAssignments, upFormula)) ->
                -- repeat until the formula is the same
                if formula == upFormula then result
                else simplify upAssignments upFormula

    -- find pure literals and add them to the assignment map. This just updates assignments and does not make a decision as
    -- to satisfiability. It works by assuming every new literal it finds is pure and then correcting as needed
    pureLiteralEliminate :: Ord a => AssignmentMap a -> Set (CNFLiteral a) -> CNFFormula a -> AssignmentMap a
    -- end of recursion
    pureLiteralEliminate assignments _ [] = assignments
    -- end of this clause, move on to the next one
    pureLiteralEliminate assignments unpure ([]:xs) = pureLiteralEliminate assignments unpure xs

    pureLiteralEliminate assignments unpure ((x:xs):ys) = let
        literalX = atomToLiteral x
        (assignments', unpure') = case (x, Map.lookup literalX assignments, Set.member literalX unpure) of
            -- something we've already marked as unpure, skip it
            (_, _, True) -> (assignments, unpure)

            -- Not in the map, add it
            (CNFAtom a, Nothing, _) -> (Map.insert a True assignments,  unpure)
            (CNFNot  a, Nothing, _) -> (Map.insert a False assignments, unpure)

            -- In the map and matches our guess, keep it
            (CNFAtom a, Just True,  _) -> (assignments, unpure)
            (CNFNot  a, Just False, _) -> (assignments, unpure)

            -- otherwise we guessed wrong. Remove from the map and add to unpure
            _ -> (Map.delete literalX assignments, Set.insert literalX unpure)
     in
        pureLiteralEliminate assignments' unpure' (xs:ys)

    unitPropagate :: Ord a => AssignmentMap a -> CNFFormula a -> Maybe (AssignmentMap a, CNFFormula a)
    -- We have a unit! If it's new, add it to assignments and eliminate the unit
    -- If it's something in assignments, check that it matches
    unitPropagate assignments ([x]:ys) = let
        literalX = atomToLiteral x
     in
        case (x, Map.lookup literalX assignments) of
            -- new literal
            (CNFAtom a, Nothing) -> unitPropagate (Map.insert a True assignments) ys
            (CNFNot  a, Nothing) -> unitPropagate (Map.insert a False assignments) ys

            -- old literal, but matches
            (CNFAtom a, Just True)  -> unitPropagate assignments ys
            (CNFNot  a, Just False) -> unitPropagate assignments ys

            -- old literal, unsolvable
            _ -> Nothing

    -- for clauses with more than one thing:
    -- if the clause contains any literal that matches the map, the whole clause is true and we can remove it
    -- otherwise, remove any literals that do not match the map, as they cannot be true. If, after removing
    -- untrue literals, the clause is empty, the expression is unsolvable.
    unitPropagate assignments (clause:ys) = let
        clauseTrue = any (\atom -> Just (atomToBool atom) == Map.lookup (atomToLiteral atom) assignments) clause
        clauseFiltered = filter (\atom -> Just (not (atomToBool atom)) == Map.lookup (atomToLiteral atom) assignments) clause
     in
        if | clauseTrue          -> unitPropagate assignments ys
           | null clauseFiltered -> Nothing
           | otherwise           -> do
                (assignments', formulaTail) <- unitPropagate assignments ys
                return (assignments', clauseFiltered:formulaTail)
