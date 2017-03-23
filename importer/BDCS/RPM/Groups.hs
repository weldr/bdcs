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

module BDCS.RPM.Groups(createGroup)
 where

import Control.Monad(forM_, void, when)
import Control.Monad.IO.Class(MonadIO)
import Data.Bits(testBit)
import Data.Char(isSpace, toLower, toUpper)
import Data.List(dropWhileEnd)
import Data.Maybe(fromJust, fromMaybe, isJust)
import Data.Word(Word32)
import Database.Esqueleto

import           BDCS.DB
import           BDCS.Groups(findRequires, findGroupRequirements)
import           BDCS.KeyValue(findKeyValue, insertKeyValue)
import qualified BDCS.ReqType as RT
import           RPM.Tags(Tag, findStringTag, findStringListTag, findTag, findWord32ListTag, tagValue)

rpmFlagsToOperator :: Word32 -> String
rpmFlagsToOperator f =
    if | f `testBit` 1 && f `testBit` 3 -> "<="
       | f `testBit` 1                  -> "<"
       | f `testBit` 2 && f `testBit` 3 -> ">="
       | f `testBit` 2                  -> ">"
       | f `testBit` 3                  -> "="
       | otherwise                      -> ""

createGroup :: MonadIO m => [Key Files] -> [Tag] -> SqlPersistT m (Key Groups)
createGroup fileIds rpm = do
    -- Get the NEVRA so it can be saved as attributes
    let epoch = findTag "Epoch" rpm >>= \t -> (tagValue t :: Maybe Word32) >>= Just . show
    let name = fromMaybe "" $ findStringTag "Name" rpm
    let version = fromMaybe "" $ findStringTag "Version" rpm
    let release = fromMaybe "" $ findStringTag "Release" rpm
    let arch = fromMaybe "" $ findStringTag "Arch" rpm

    -- Create the groups row
    groupId <- insert $ Groups name "rpm"

    -- Create the group_files rows
    void $ mapM (\fId -> insert $ GroupFiles groupId fId) fileIds

    -- Create the (E)NVRA attributes
    -- FIXME could at least deduplicate name and arch real easy
    forM_ [("name", name), ("version", version), ("release", release), ("arch", arch)] $ \(k, v) ->
        findKeyValue k v Nothing >>= \case
            Nothing -> insertKeyValue k v Nothing >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    -- Add the epoch attribute, when it exists.
    when (isJust epoch) $ void $ do
        let (k, v) = ("epoch", fromJust epoch)
        findKeyValue k v Nothing >>= \case
            Nothing -> insertKeyValue k v Nothing >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    forM_ [("Provide", "rpm-provide"), ("Conflict", "rpm-conflict"), ("Obsolete", "rpm-obsolete"), ("Order", "rpm-install-after")] $ \tup ->
        basicAddPRCO rpm groupId (fst tup) (snd tup)

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
    basicAddPRCO tags groupId tagBase keyName =
        addPRCO tagBase tags $ \expr -> let
            -- split out the name part of "name >= version"
            exprBase = takeWhile (/= ' ')  expr
          in
            findKeyValue keyName exprBase (Just expr) >>= \case
                Nothing -> insertKeyValue keyName exprBase (Just expr) >>= \kvId -> insert $ GroupKeyValues groupId kvId
                Just kv -> insert $ GroupKeyValues groupId kv

    addPRCO :: Monad m => String -> [Tag] -> (String -> m a) -> m ()
    addPRCO ty tags fn = do
        let names = findStringListTag (ty' ++ "Name") tags
        let flags = findWord32ListTag (ty' ++ "Flags") tags
        let vers  = findStringListTag (ty' ++ "Version") tags

        -- TODO How to handle everything from RPMSENSE_POSTTRANS and beyond?
        forM_ (zip3 names flags vers) $ \(n, f, v) -> do
            let cmp  = rpmFlagsToOperator f
            let expr = dropWhileEnd isSpace $ n ++ " " ++ cmp ++ " " ++ v

            fn expr
     where
        ty' = titlecase ty

    titlecase :: String -> String
    titlecase (hd:rest) = toUpper hd : map toLower rest
    titlecase []        = []
