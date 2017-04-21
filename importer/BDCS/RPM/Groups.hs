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

{-# LANGUAGE LambdaCase #-}
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
import           BDCS.Groups(findRequires, findGroupRequirements)
import           BDCS.KeyValue(findKeyValue, insertKeyValue)
import qualified BDCS.ReqType as RT
import           RPM.Tags(Tag, findStringTag, findStringListTag, findTag, findWord32ListTag, tagValue)

rpmFlagsToOperator :: Word32 -> T.Text
rpmFlagsToOperator f =
    if | f `testBit` 1 && f `testBit` 3 -> "<="
       | f `testBit` 1                  -> "<"
       | f `testBit` 2 && f `testBit` 3 -> ">="
       | f `testBit` 2                  -> ">"
       | f `testBit` 3                  -> "="
       | otherwise                      -> ""

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
    forM_ [("name", name), ("version", version), ("release", release), ("arch", arch)] $
        insertKeyValueIfMissing groupId

    -- Add the epoch attribute, when it exists.
    when (isJust epoch) $ void $
        insertKeyValueIfMissing groupId ("epoch", fromJust epoch)

    forM_ [("Provide", "rpm-provide"), ("Conflict", "rpm-conflict"), ("Obsolete", "rpm-obsolete"), ("Order", "rpm-install-after")] $ \tup ->
        uncurry (basicAddPRCO rpm groupId) tup

    -- Create the Requires attributes
    forM_ [("Require", RT.Must), ("Recommend", RT.Should), ("Suggest", RT.May),
           ("Supplement", RT.ShouldIfInstalled), ("Enhance", RT.MayIfInstalled)] $ \tup ->
        addPRCO (fst tup) rpm $ \expr -> do
            reqId <- findRequires RT.RPM RT.Runtime (snd tup) expr >>= \case
                         Nothing  -> insert $ Requirements RT.RPM RT.Runtime (snd tup) expr
                         Just rid -> return rid

            -- Don't insert a requirement for a group more than once.  RPMs can have the same
            -- requirement listed multiple times, for whatever reason, but we want to reduce
            -- duplication.
            findGroupRequirements groupId reqId >>= \case
                Nothing -> void $ insert $ GroupRequirements groupId reqId
                Just _  -> return ()

    return groupId
 where
    insertKeyValueIfMissing groupId (k, v) =
        findKeyValue k v Nothing >>= \case
            Nothing -> insertKeyValue k v Nothing >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    basicAddPRCO tags groupId tagBase keyName =
        addPRCO tagBase tags $ \expr -> let
            -- split out the name part of "name >= version"
            exprBase = T.takeWhile (/= ' ')  expr
          in
            findKeyValue keyName exprBase (Just expr) >>= \case
                Nothing -> insertKeyValue keyName exprBase (Just expr) >>= \kvId -> insert $ GroupKeyValues groupId kvId
                Just kv -> insert $ GroupKeyValues groupId kv

    addPRCO :: Monad m => T.Text -> [Tag] -> (T.Text -> m a) -> m ()
    addPRCO ty tags fn = do
        let names = map T.pack $ findStringListTag (T.unpack ty' ++ "Name") tags
        let flags =              findWord32ListTag (T.unpack ty' ++ "Flags") tags
        let vers  = map T.pack $ findStringListTag (T.unpack ty' ++ "Version") tags

        -- TODO How to handle everything from RPMSENSE_POSTTRANS and beyond?
        forM_ (zip3 names flags vers) $ \(n, f, v) -> do
            let cmp  = rpmFlagsToOperator f
            let expr = T.stripEnd $ T.concat [n, " ", cmp, " ", v]

            fn expr
     where
        ty' = T.toTitle ty
