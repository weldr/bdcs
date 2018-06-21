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

{-# LANGUAGE FlexibleContexts #-}

module BDCS.NPM.SemVer(SemVer(..),
                       SemVerIdentifier(..),
                       SemVerRangePart,
                       SemVerRange,
                       SemVerRangeSet,
                       parseSemVer,
                       parseSemVerRangeSet,
                       satisfies,
                       toText)
 where

import           Control.Monad(void)
import           Data.Char(isAsciiLower, isAsciiUpper, isDigit)
import           Data.List(intersperse)
import           Data.Monoid((<>))
import qualified Data.Text as T
import           Text.Parsec
import           Text.Parsec.Error(Message(..), newErrorMessage)
import           Text.Parsec.Pos(initialPos)
import           Text.Parsec.Text(Parser)

-- why not use the semver package?
-- As of semver-0.3.3.1, the following problem exists:
--   let v = version 1 2 3 [] []
--   in  compare v v == GT
--
-- once that gets fixed, we can maybe use it.
-- Another potential issue is that the integer type used for major/minor/patch (Int)
-- is more restricted than what javascript's semver allows

-- | A Semantic version, as defined by http://semver.org/
data SemVer = SemVer { major      :: Integer,
                       minor      :: Integer,
                       patch      :: Integer,
                       preRelease :: [SemVerIdentifier],
                       buildMeta  :: [SemVerIdentifier]
                     } deriving (Eq, Show)

instance Ord SemVer where
    compare v1 v2 = major v1 `compare` major v2 <>
                    minor v1 `compare` minor v2 <>
                    patch v1 `compare` patch v2 <>
                    comparePreRelease (preRelease v1) (preRelease v2)
     where
        -- An empty pre-release is greater than a non-empty pre-releases.
        -- If both versions have a pre-release, the comparison is the same as for 'List':
        -- compare each element, the first one with a greater element wins, otherwise longer list is greater
        comparePreRelease :: [SemVerIdentifier] -> [SemVerIdentifier] -> Ordering
        comparePreRelease [] (_:_) = GT
        comparePreRelease (_:_) [] = LT
        comparePreRelease l1 l2 = l1 `compare` l2

-- | a component of a pre-release or buildmeta identifier list
data SemVerIdentifier = NumericIdentifier Integer
                      | TextIdentifier T.Text
 deriving(Eq, Show)

instance Ord SemVerIdentifier where
    -- numeric identifiers are less than text identifiers
    compare (NumericIdentifier _)  (TextIdentifier _)     = LT
    compare (TextIdentifier _)     (NumericIdentifier _)  = GT
    compare (NumericIdentifier n1) (NumericIdentifier n2) = n1 `compare` n2
    compare (TextIdentifier t1)    (TextIdentifier t2)    = t1 `compare` t2

-- | Parse a semantic version
parseSemVer :: T.Text -> Either ParseError SemVer
-- reuse the PartialSemVer parser and reject wildcards
parseSemVer input = case parse justPartialParsec "" input of
    Left e    -> Left e
    Right PartialSemVer{partialMajor=Just major,
                        partialMinor=Just minor,
                        partialPatch=Just patch,
                        ..} -> Right $ SemVer major minor patch partialPreReleaseTags partialBuildMeta
    Right _   -> Left $ newErrorMessage (Message "Wildcards not permitted in SemVer") (initialPos "")
 where
    justPartialParsec :: Parser PartialSemVer
    justPartialParsec = do
        p <- partialParsec
        eof
        return p

initialVer :: SemVer
initialVer = SemVer 0 0 0 [] []

-- | A single version condition, e.g. >= 1.0.0.
-- To satisfy the condition, a 'SemVer' must match at least one of
-- the 'Ordering's.
type SemVerRangePart = [(Ordering, SemVer)]

-- | A range of semantic versions. To satisfy the range, a 'SemVer' must
-- satisfy every element of the list.
type SemVerRange = [SemVerRangePart]

-- | A set of semantic version ranges. To satisfy the set, a 'SemVer' must
-- satisfy at least one of the ranges in the list.
type SemVerRangeSet = [SemVerRange]

-- | Parse a SemVer range set according to the npm syntax.
parseSemVerRangeSet :: T.Text -> Either ParseError SemVerRangeSet
parseSemVerRangeSet input = parse rangeSet "" input
 where
    -- lexical elements
    logicalOr :: Parser ()
    logicalOr = void $ string "||"

    hyphen :: Parser ()
    hyphen = void $ char '-'

    gt :: Parser ()
    gt = void $ char '>'

    gte :: Parser ()
    gte = void $ string ">="

    eq :: Parser ()
    eq = void $ char '='

    lte :: Parser ()
    lte = void $ string "<="

    lt :: Parser ()
    lt = void $ char '<'

    tilde :: Parser ()
    tilde = void $ char '~'

    caret :: Parser ()
    caret = void $ char '^'

    operator :: Parser SemVerOrdering
    operator =
        (try gte >> return GreaterThanEqual) <|>
        (gt      >> return GreaterThan)     <|>
        (eq      >> return EqualTo)     <|>
        (try lte >> return LessThanEqual) <|>
        (lt      >> return LessThan)

    operatorToOrdering :: SemVerOrdering -> [Ordering]
    operatorToOrdering GreaterThanEqual = [GT, EQ]
    operatorToOrdering GreaterThan      = [GT]
    operatorToOrdering EqualTo          = [EQ]
    operatorToOrdering LessThanEqual    = [LT, EQ]
    operatorToOrdering LessThan         = [LT]

    -- range set: range (|| range)*
    rangeSet :: Parser SemVerRangeSet
    rangeSet = do
        set <- range `sepBy1` try (spaces >> logicalOr >> spaces)
        eof

        return set

    -- range: lower-upper, or list of versions separated by spaces, or an empty string
    range :: Parser SemVerRange
    range =
        try hyphenRange <|>
        try simpleList  <|>
        return [[(EQ, initialVer), (GT, initialVer)]]

    -- lower-upper
    hyphenRange :: Parser SemVerRange
    hyphenRange = do
        lowerVersion <- partialParsec
        spaces
        hyphen
        spaces
        upperVersion <- partialParsec

        return [partialToLowerRange lowerVersion, partialToUpperRange upperVersion]

    -- a list of individual versions
    simpleList :: Parser SemVerRange
    simpleList = do
        s <- simpleParsec
        l <- option [] $ try (spaces >> simpleList)
        return (s ++ l)

    simpleParsec :: Parser SemVerRange
    simpleParsec =
        primitive    <|>
        tildeVersion <|>
        caretVersion <|>
        standaloneVersion

    -- This is a standalone operator + version, e.g., ">= 1.0"
    -- For exact versions, it's just (operator) (version)
    -- For wildcard versions:
    --   - =  p is the same as a standalone version, i.e. p - p
    --   - >  p is >= (partialToUpper p)
    --   - <  p is <  (partialToLower p)
    --   - <= p is <  (partialToUpper p)
    --   - >= p is >= (partialToLower p)
    -- Any missing parts of the PartialSemVer are replaced with 0.
    primitive :: Parser SemVerRange
    primitive = do
        ordering <- operator
        spaces
        p <- partialParsec

        return $ if isExact p then
            [map (\o -> (o, partialToLower p)) (operatorToOrdering ordering)]
         else
            case ordering of
                EqualTo          -> [partialToLowerRange p, partialToUpperRange p]
                GreaterThan      -> [[(GT, partialToUpper p), (EQ, partialToUpper p)]]
                LessThan         -> [[(LT, partialToLower p)]]
                LessThanEqual    -> [[(LT, partialToUpper p)]]
                GreaterThanEqual -> [[(GT, partialToLower p), (EQ, partialToLower p)]]

    -- patch-level changes if a minor is specified, minor-level changes if not
    -- I don't know why, but npm allows '~ > ver', which is the same as '~ ver'
    tildeVersion :: Parser SemVerRange
    tildeVersion = do
        tilde
        spaces
        optional gt
        spaces
        version <- partialParsec

        return [partialToLowerRange version, partialToTilde version]

    -- Allow changes that do not modify the left-most non-zero digit of the version
    caretVersion :: Parser SemVerRange
    caretVersion = do
        caret
        spaces
        version <- partialParsec

        return [partialToLowerRange version, partialToCaret version]

    -- Just a version: this is equivalent to the range <version> - <version>
    standaloneVersion :: Parser SemVerRange
    standaloneVersion = do
        version <- partialParsec
        return [partialToLowerRange version, partialToUpperRange version]

-- | Whether a given version satisfies a given range.
--
-- When the version contains pre-release tags, it only satisifes a SemVerRange
-- if at least one version in the range has a matching major.minor.patch version
-- number and also contains pre-release tags.
satisfies :: SemVer -> SemVerRangeSet -> Bool
satisfies v1 set = any satisfiesRange set
 where
    satisfiesRange :: SemVerRange -> Bool
    -- if v1 has no pre-release tags, we can just compare versions the normal way.
    -- otherwise, the version only satisfies the range if there is match maj.min.patch with a pre-release
    satisfiesRange range = let
        normalCase     = all satisfiesPart range
        preReleaseCase = any matchesPart range
        isPreRelease   = (not . null) (preRelease v1)
     in
        if isPreRelease then
            normalCase && preReleaseCase
        else
            normalCase

    satisfiesPart :: SemVerRangePart -> Bool
    satisfiesPart = any satisfiesExpr

    satisfiesExpr :: (Ordering, SemVer) -> Bool
    satisfiesExpr (o, v2) = compare v1 v2 == o

    matchesPart :: SemVerRangePart -> Bool
    matchesPart = any matchesExpr

    matchesExpr :: (Ordering, SemVer) -> Bool
    matchesExpr (_, v2) = (not . null) (preRelease v2) &&
                          (major v1 == major v2) &&
                          (minor v1 == minor v2) &&
                          (patch v1 == patch v2)

toText :: SemVer -> T.Text
toText SemVer{..} = let
    mainver  = [T.pack $ show major, ".",
                T.pack $ show minor, ".",
                T.pack $ show patch]
    prever   = if null preRelease then [] else "-":idsToText preRelease
    buildver = if null buildMeta  then [] else "+":idsToText buildMeta
 in
    T.concat $ mainver ++ prever ++ buildver
 where
    idsToText ids = intersperse "." $ map idToText ids

    idToText (NumericIdentifier i) = T.pack $ show i
    idToText (TextIdentifier t)    = t

-- Internal type for parsing partial versions
data PartialSemVer = PartialSemVer {
    partialMajor :: Maybe Integer,
    partialMinor :: Maybe Integer,
    partialPatch :: Maybe Integer,
    partialPreReleaseTags :: [SemVerIdentifier],
    partialBuildMeta :: [SemVerIdentifier]
 } deriving (Show)

-- Token type for Ordering, plus >= and <=
data SemVerOrdering = LessThan
                    | EqualTo
                    | GreaterThan
                    | LessThanEqual
                    | GreaterThanEqual

partialParsec :: Parser PartialSemVer
partialParsec = do
    let integer = Just <$> read <$> many1 digit
    let parseWildCard = try integer <|>
                        (oneOf "xX*" >> return Nothing)

    -- allow the version to start with any combination of 'v', '=' or space characters
    skipMany $ oneOf "v=" <|> space

    -- major is required, everything else is optional
    major <- parseWildCard
    minor <- option Nothing (char '.' >> parseWildCard)

    -- prerelease/buildmeta are also optional, but can only occur if patch is present
    (patch, preRelease, buildMeta) <-
        option (Nothing, [], []) $ do
            patch <- char '.' >> parseWildCard

            -- pre-release is supposed to be preceded by a hyphen (1.2.3-pre), but
            -- npm's "loose" mode allows the hyphen to be skipped (1.2.3pre)

            preRelease <- option [] (optional (char '-') >> parseExtra)
            buildMeta  <- option [] (char '+' >> parseExtra)
            return (patch, preRelease, buildMeta)

    spaces

    return $ PartialSemVer major minor patch preRelease buildMeta
 where
    -- parse a pre-release or buildmeta item list
    parseExtra :: Parser [SemVerIdentifier]
    parseExtra = parseExtraItem `sepBy1` char '.'

    parseExtraItem :: Parser SemVerIdentifier
    parseExtraItem = do
        str <- many1 (satisfy isAsciiLower <|>
                      satisfy isAsciiUpper <|>
                      satisfy isDigit      <|>
                      char '-')

        return $ if all isDigit str then
            NumericIdentifier (read str)
         else
            TextIdentifier (T.pack str)

isExact :: PartialSemVer -> Bool
isExact PartialSemVer{partialMajor=(Just _),
                      partialMinor=(Just _),
                      partialPatch=(Just _),
                      ..} = True
isExact _ = False

-- Convert a wildcard semver to the lowest possible matching version. i.e., repalce wildcards with 0.
-- Anything after a missing piece is ignored, so something like 1.*.7 becomes 1.0.0
partialToLower :: PartialSemVer -> SemVer
partialToLower PartialSemVer{..} = let
    (major, minor, patch, prerelease) = case (partialMajor, partialMinor, partialPatch) of
        (Nothing, _, _)                                  -> (0, 0, 0, [])
        (Just partMajor, Nothing, _)                     -> (partMajor, 0, 0, [])
        (Just partMajor, Just partMinor, Nothing)        -> (partMajor, partMinor, 0, [])
        (Just partMajor, Just partMinor, Just partPatch) -> (partMajor, partMinor, partPatch, partialPreReleaseTags)
 in
    SemVer major minor patch prerelease []

-- Convert a wildcard semver to the lowest version that is greater than the wildcard.
-- Increment the last known component and replace the remaining components with 0.
-- This should not be used with exact versions.
partialToUpper :: PartialSemVer -> SemVer
partialToUpper PartialSemVer{partialMajor=Nothing} = initialVer
partialToUpper PartialSemVer{partialMajor=(Just major),
                             partialMinor=Nothing,
                             ..}                   = SemVer (major+1) 0 0 [] []
partialToUpper PartialSemVer{partialMajor=(Just major),
                             partialMinor=(Just minor),
                             partialPatch=Nothing,
                             ..}                   = SemVer major (minor+1) 0 [] []
partialToUpper PartialSemVer{partialMajor=(Just major),
                             partialMinor=(Just minor),
                             partialPatch=(Just patch),
                             ..}                   = SemVer major minor (patch+1) [] []

-- use a partial semver as the lower end of a range, >= p
partialToLowerRange :: PartialSemVer -> SemVerRangePart
partialToLowerRange p = let
    semver = partialToLower p
 in
    [(EQ, semver), (GT, semver)]

-- use a partial semver as the upper end of a range
-- For exact versions, this is <= p
-- For partial versions, this is < (partialToUpper) p
partialToUpperRange :: PartialSemVer -> SemVerRangePart

-- all wildcard, return >= 0.0.0 as an always-true condition
partialToUpperRange PartialSemVer{partialMajor=Nothing, ..} = [(GT, initialVer), (EQ, initialVer)]

-- No wildcard parts
partialToUpperRange PartialSemVer{partialMajor=(Just major),
                                  partialMinor=(Just minor),
                                  partialPatch=(Just patch),
                                  ..} =
    let s = SemVer major minor patch partialPreReleaseTags []
    in  [(EQ, s), (LT, s)]

-- some wildcard parts
partialToUpperRange p = [(LT, partialToUpper p)]

-- For tilde ranges: allow patch-level changes if a minor version is specified,
-- minor-level changes if not
-- ~x.y.z  => x.y.z - x.y.*
-- ~x.y.*  => x.y.*
-- ~x.*.*  => x.*
-- ~*      => *
--
-- This function returns the upper expression of the range
partialToTilde :: PartialSemVer -> SemVerRangePart
partialToTilde PartialSemVer{partialPatch=(Just _), ..} = partialToUpperRange $ PartialSemVer partialMajor partialMinor Nothing [] []
partialToTilde p                                        = partialToUpperRange p

-- For caret ranges, allow changes that do not modify the left-most non-zero digit of the version
-- ^1.2.3 => 1.2.3 - 1.*
-- ^0.2.3 => 0.2.3 - 0.2.*
-- ^0.0.3 => >=0.0.3 && <0.0.4 (i.e., = 0.0.3)
--
-- In the case of partial versions, the patch version is always allowed to
-- change even if major and minor are zero.
--
-- ^1.2.* => ^1.2.0 => 1.2.0 - 1.*
-- ^0.2.* => ^0.2.0 => 0.2.0 - 0.2.*
-- ^0.0.* => 0.0.0 - 0.0.* => 0.0.*
--
-- This function returns the upper expression of the range
partialToCaret :: PartialSemVer -> SemVerRangePart

-- the special case, 0.0.<patch>: if patch is a wildcard, leave it as a wildcard so patch level can change
--                                if patch is a version, leave the restriction in place
partialToCaret PartialSemVer{partialMajor=(Just 0), partialMinor=(Just 0), ..} = partialToUpperRange $ PartialSemVer (Just 0) (Just 0) partialPatch [] []

-- 0 major, wildcard or non-zero minor:
--  if minor is non-zero, leave it and allow patch to change
--  if minor is a wildcard, allow minor to change by leaving it a wildcard
partialToCaret PartialSemVer{partialMajor=(Just 0), ..} = partialToUpperRange $ PartialSemVer (Just 0) partialMinor Nothing [] []

-- non-zero or wildcard major:
-- if non-zero, leave major and allow minor and patch to change
-- if wildcard, leave it as a wildcard
partialToCaret PartialSemVer{..} = partialToUpperRange $ PartialSemVer partialMajor Nothing Nothing [] []
