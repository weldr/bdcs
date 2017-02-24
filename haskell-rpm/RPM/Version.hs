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

module RPM.Version(vercmp)
 where

import Data.Char(isAsciiLower, isAsciiUpper, isDigit)

vercmp :: String -> String -> Ordering
vercmp a b = let
    -- strip out all non-version characters
    -- keep in mind the strings may be empty after this
    a' = dropSeparators a
    b' = dropSeparators b
  in
    case (a', b') of
        -- Nothing left means the versions are equal
        ([], [])   -> EQ
        -- tilde ls less than everything, including an empty string
        ('~':aTail, '~':bTail) -> vercmp aTail bTail
        ('~':_, _) -> LT
        (_, '~':_) -> GT
        -- otherwise, if one of the strings is null, the other is greater
        ([], _)    -> LT
        (_, [])    -> GT
        -- Now we have two non-null strings, starting with a non-tilde version character
        _          -> let 
            -- rpm compares strings by digit and non-digit components, so grab the first
            -- component of one type
            fn = if isDigit (head a') then isDigit else isAsciiAlpha
            (prefixA, suffixA) = span fn a'
            (prefixB, suffixB) = span fn b'
         in
            -- if one prefix is a number and the other is a string, the one
            -- that is a number is the more recent version number
            if | isDigit (head a') && (not . isDigit) (head b') -> GT
               | (not . isDigit) (head a') && isDigit (head b') -> LT
               | isDigit (head a') -> (prefixA `compareAsInts` prefixB) `mappend` (suffixA `vercmp` suffixB)
               | otherwise -> (prefixA `compare` prefixB) `mappend` (suffixA `vercmp` suffixB)
  where
    compareAsInts :: String -> String -> Ordering
    -- the version numbers can overflow Int, so strip leading 0's and do a string compare,
    -- longest string wins
    compareAsInts x y =
        let x' = dropWhile (== '0') x
            y' = dropWhile (== '0') y
        in 
            if length x' > length y' then GT
            else x' `compare` y'

    -- isAlpha returns any unicode alpha, but we just want ASCII characters
    isAsciiAlpha :: Char -> Bool
    isAsciiAlpha x = isAsciiLower x || isAsciiUpper x

    -- RPM only cares about ascii digits, ascii alpha, and ~
    isVersionChar :: Char -> Bool
    isVersionChar x = isDigit x || isAsciiAlpha x || x == '~'

    dropSeparators :: String -> String
    dropSeparators = dropWhile (not . isVersionChar)
