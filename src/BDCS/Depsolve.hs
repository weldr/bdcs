{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- |
-- Module: BDCS.Depsolve
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Manage 'Builds' records in the database.

module BDCS.Depsolve(Formula(..),
                     CNFLiteral(..),
                     CNFAtom(..),
                     CNFFormula,
                     DepAssignment,
                     formulaToCNF,
                     solveCNF
-- export private symbols for testing
#ifdef TEST
                   , pureLiteralEliminate
                   , unitPropagate
#endif
                     )
 where

import           Control.Monad.Except(MonadError, catchError, throwError)
import           Control.Monad.State(State, evalState, state)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe(isNothing, mapMaybe)
import           Data.Set(Set)
import qualified Data.Set as Set

import BDCS.Utils.Monad(concatMapM)

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

formulaToCNF :: Formula a -> CNFFormula a
formulaToCNF f =
    -- wrap the call in a State Int starting at 0 to create a counter for substitution variables
    evalState (formulaToCNF' f) 0
 where
    formulaToCNF' :: Formula a -> State Int (CNFFormula a)

    -- easy ones: a becomes AND(OR(a)), NOT(a) becomes AND(OR(NOT(a)))
    formulaToCNF' (Atom x) = return [[CNFAtom (CNFOriginal x)]]
    formulaToCNF' (Not x)  = return [[CNFNot (CNFOriginal x)]]

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
    formulaToCNF' (And andFormulas) = concatMapM formulaToCNF' andFormulas

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
    formulaToCNF' (Or []) = return [[]]
    formulaToCNF' (Or [x]) = formulaToCNF' x

    formulaToCNF' (Or (x:xs)) = do
        -- Get and increment the counter
        subVar <- state $ \i -> (CNFSubstitute i, i+1)

        -- recurse on the left hand expression
        lhCNF <- formulaToCNF' x

        -- distribute NOT(subVar) AND lhCNF by adding NOT(subVar) into each of the OR lists
        let lhSubCNF = map (CNFNot subVar:) lhCNF

        -- recurse on the right hand side
        rhCNF <- formulaToCNF' (Or xs)

        -- distribute subVar across the right hand expression
        let rhSubCNF = map (CNFAtom subVar:) rhCNF

        -- combine the results
        return (lhSubCNF ++ rhSubCNF)

-- assignments to literals that will satisfy a formula
type DepAssignment a = (a, Bool)

-- internal types for the variable=bool assignments
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
solveCNF :: (MonadError String m, Ord a) => CNFFormula a -> m [DepAssignment a]
solveCNF formula = solveCNF' Map.empty formula
 where
    -- helper function that takes an assignment map and a formula
    solveCNF' :: (MonadError String m, Ord a) => AssignmentMap a -> CNFFormula a -> m [DepAssignment a]
    solveCNF' assignments f =
        -- simplify the formula. simplify will recurse as necessary
        simplify assignments f >>= \case
            -- All clauses have been satisfied, we're done. Return the assignments
            (assignments', [])       -> return $ assignmentsToList assignments'
            -- otherwise, try an assignment, or if that fails try the opposite assignment
            (assignments', formula') -> guessAndCheck assignments' formula'

    guessAndCheck :: (MonadError String m, Ord a) => AssignmentMap a -> CNFFormula a -> m [DepAssignment a]
    guessAndCheck assignments f@((firstLiteral:_):_) =
        try True `catchError` const (try False)
     where
        try val = do
            let tryAssignments = Map.insert (atomToLiteral firstLiteral) val assignments
            solveCNF' tryAssignments f

    -- probably shouldn't happen
    guessAndCheck assignments ([]:ys) = guessAndCheck assignments ys

    -- No variables left, so we're done
    guessAndCheck _ [] = return []

    simplify :: (MonadError String m, Ord a) => AssignmentMap a -> CNFFormula a -> m (AssignmentMap a, CNFFormula a)
    simplify assignments f = do
        -- pureLiteralEliminate only updates the assignments, the assigned literals are actually
        -- removed by unitPropagate.
        let pleAssignments = pureLiteralEliminate Set.empty assignments f

        (upAssignments, upFormula) <- unitPropagate pleAssignments f

        -- repeat until the formula doesn't change
        if f == upFormula then
            return (upAssignments, upFormula)
        else
            simplify upAssignments upFormula

    assignmentsToList :: Ord a => AssignmentMap a -> [DepAssignment a]
    assignmentsToList assignments = let
        -- start by getting everything out of the map as a list of (CNFLiteral, Bool)
        literalList = Map.assocs assignments
     in
         -- map each (literal, bool) to Maybe (orig, bool), mapMaybe will filter out the Nothings
        mapMaybe literalToOriginal literalList
     where
        -- unwrap original values, discard substitutes
        literalToOriginal :: (CNFLiteral a, Bool) -> Maybe (a, Bool)
        literalToOriginal (CNFOriginal x, b) = Just (x, b)
        literalToOriginal _                  = Nothing

-- find pure literals and add them to the assignment map. This just updates assignments and does not make a decision as
-- to satisfiability. It works by assuming every new literal it finds is pure and then correcting as needed. The Set
-- argument is the literals that have been found to be unpure (i.e, they appear as both A and ~A)
pureLiteralEliminate :: Ord a => Set (CNFLiteral a) -> AssignmentMap a -> CNFFormula a -> AssignmentMap a

-- end of recursion
pureLiteralEliminate _ assignments [] = assignments
-- end of a clause, move on to the next one
pureLiteralEliminate unpure assignments ([]:ys) = pureLiteralEliminate unpure assignments ys

-- update unpure and assignments based on the first element and continue
pureLiteralEliminate unpure assignments ((x:xs):ys) = let
    (unpure', assignments') = updateAssignments
 in
    pureLiteralEliminate unpure' assignments' (xs:ys)
 where
    updateAssignments = let
        literalX = atomToLiteral x
     in
        case (x, Map.lookup literalX assignments, Set.member literalX unpure) of
            -- something we've already marked as unpure, skip it
            (_, _, True) -> (unpure, assignments)

            -- Not in the map, add it
            (CNFAtom a, Nothing, _) -> (unpure, Map.insert a True assignments)
            (CNFNot  a, Nothing, _) -> (unpure, Map.insert a False assignments)

            -- In the map and matches our guess, keep it
            (CNFAtom _, Just True,  _) -> (unpure, assignments)
            (CNFNot  _, Just False, _) -> (unpure, assignments)

            -- otherwise we guessed wrong. Remove from the map and add to unpure
            _ -> (Set.insert literalX unpure, Map.delete literalX assignments)

unitPropagate :: (MonadError String m, Ord a) => AssignmentMap a -> CNFFormula a -> m (AssignmentMap a, CNFFormula a)

-- We have a unit! If it's new, add it to assignments and eliminate the unit
-- If it's something in assignments, check that it matches
unitPropagate assignments ([x]:ys) = do
    let literalX = atomToLiteral x
    let boolX = atomToBool x
    let literalLookup = Map.lookup literalX assignments

                       -- if literalLookup is Nothing, this is a new literal. add it to the assignments.
    assignments' <- if | isNothing literalLookup     -> return $ Map.insert literalX boolX assignments
                       -- old literal, matches
                       | Just boolX == literalLookup -> return assignments
                       -- old literal, does not match
                       | otherwise                   -> throwError "Unable to solve expression"

    unitPropagate assignments' ys

-- for clauses with more than one thing:
-- if the clause contains any literal that matches the map, the whole clause is true and we can remove it
-- otherwise, remove any literals that do not match the map, as they cannot be true. If, after removing
-- untrue literals, the clause is empty, the expression is unsolvable.
unitPropagate assignments (clause:ys) = do
    let clauseTrue = any (\atom -> Just (atomToBool atom) == Map.lookup (atomToLiteral atom) assignments) clause
    let clauseFiltered = filter (\atom -> case Map.lookup (atomToLiteral atom) assignments of
                                            Nothing -> True
                                            Just x  -> x == atomToBool atom) clause

    if | clauseTrue          -> unitPropagate assignments ys
       | null clauseFiltered -> throwError "Unable to solve expression"
       | otherwise           -> do
            (assignments', formula') <- unitPropagate assignments ys
            return (assignments', clauseFiltered:formula')

unitPropagate assignments [] = return (assignments, [])

-- unwrap an atom
atomToLiteral :: CNFAtom a -> CNFLiteral a
atomToLiteral (CNFAtom x) = x
atomToLiteral (CNFNot x)  = x

atomToBool :: CNFAtom a -> Bool
atomToBool (CNFAtom _) = True
atomToBool (CNFNot _)  = False
