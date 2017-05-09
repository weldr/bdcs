-- Copyright (C) 2016-2017 Red Hat, Inc.
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
{-# LANGUAGE OverloadedStrings #-}

module BDCS.RPM.Groups(createGroup)
 where

import           Control.Monad(forM_, void, when)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Bits(testBit)
import           Data.Maybe(fromJust, isJust)
import qualified Data.Text as T
import           Data.Word(Word32)
import           Database.Esqueleto

import           BDCS.DB
import           BDCS.GroupKeyValue(insertGroupKeyValue)
import           BDCS.Requirements(insertGroupRequirement, insertRequirement)
import qualified BDCS.ReqType as RT
import           BDCS.RPM.Requirements(mkGroupRequirement, mkRequirement)
import           RPM.Tags(Tag, findStringTag, findStringListTag, findTag, findWord32ListTag, tagValue)

addPRCO :: MonadIO m => [Tag] -> Key Groups -> T.Text -> T.Text -> SqlPersistT m ()
addPRCO tags groupId tagBase keyName =
    withPRCO tagBase tags $ \expr -> let
        -- split out the name part of "name >= version"
        exprBase = T.takeWhile (/= ' ')  expr
      in
        insertGroupKeyValue keyName exprBase (Just expr) groupId

prcoExpressions :: T.Text -> [Tag] -> [T.Text]
prcoExpressions ty tags = let
    ty'   = T.toTitle ty

    names = map T.pack $ findStringListTag (T.unpack ty' ++ "Name") tags
    flags =              findWord32ListTag (T.unpack ty' ++ "Flags") tags
    vers  = map T.pack $ findStringListTag (T.unpack ty' ++ "Version") tags
 in
    map (\(n, f, v) -> T.stripEnd $ T.concat [n, " ", rpmFlagsToOperator f, " ", v])
        (zip3 names flags vers)

rpmFlagsToOperator :: Word32 -> T.Text
rpmFlagsToOperator f =
    if | f `testBit` 1 && f `testBit` 3 -> "<="
       | f `testBit` 1                  -> "<"
       | f `testBit` 2 && f `testBit` 3 -> ">="
       | f `testBit` 2                  -> ">"
       | f `testBit` 3                  -> "="
       | otherwise                      -> ""

withPRCO :: Monad m => T.Text -> [Tag] -> (T.Text -> m a) -> m ()
withPRCO ty tags fn =
    mapM_ fn (prcoExpressions ty tags)

-- Ignore the suggestion to not use lambda for creating GroupFiles rows, since
-- the lambda makes it more clear what's actually happening
{-# ANN createGroup ("HLint: ignore Avoid lambda" :: String) #-}
createGroup :: MonadIO m => [Key Files] -> [Tag] -> SqlPersistT m (Key Groups)
createGroup fileIds rpm = do
    -- Get the NEVRA so it can be saved as attributes
    let epoch   = findTag "Epoch" rpm >>= \t -> (tagValue t :: Maybe Word32) >>= Just . T.pack . show
    let name    = maybe "" T.pack (findStringTag "Name" rpm)
    let version = maybe "" T.pack (findStringTag "Version" rpm)
    let release = maybe "" T.pack (findStringTag "Release" rpm)
    let arch    = maybe "" T.pack (findStringTag "Arch" rpm)

    -- Create the groups row
    groupId <- insert $ Groups name "rpm"

    -- Create the group_files rows
    mapM_ (\fId -> insert $ GroupFiles groupId fId) fileIds

    -- Create the (E)NVRA attributes
    -- FIXME could at least deduplicate name and arch real easy
    forM_ [("name", name), ("version", version), ("release", release), ("arch", arch)] $ \tup ->
        uncurry insertGroupKeyValue tup Nothing groupId

    -- Add the epoch attribute, when it exists.
    when (isJust epoch) $ void $
        insertGroupKeyValue "epoch" (fromJust epoch) Nothing groupId

    forM_ [("Provide", "rpm-provide"), ("Conflict", "rpm-conflict"), ("Obsolete", "rpm-obsolete"), ("Order", "rpm-install-after")] $ \tup ->
        uncurry (addPRCO rpm groupId) tup

    -- Create the Requires attributes
    forM_ [("Require", RT.Must), ("Recommend", RT.Should), ("Suggest", RT.May),
           ("Supplement", RT.ShouldIfInstalled), ("Enhance", RT.MayIfInstalled)] $ \tup ->
        withPRCO (fst tup) rpm $ \expr -> do
            reqId <- insertRequirement $ mkRequirement (snd tup) expr

            -- Don't insert a requirement for a group more than once.  RPMs can have the same
            -- requirement listed multiple times, for whatever reason, but we want to reduce
            -- duplication.
            void $ insertGroupRequirement $ mkGroupRequirement groupId reqId

    return groupId
