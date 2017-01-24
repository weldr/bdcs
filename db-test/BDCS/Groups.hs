-- Copyright (C) 2016 Red Hat, Inc.
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

module BDCS.Groups(createGroup,
                   findRequires)
 where

import Control.Monad(forM_, void, when)
import Control.Monad.IO.Class(MonadIO)
import Data.Bits(testBit)
import Data.Char(isSpace)
import Data.List(dropWhileEnd)
import Data.Maybe(fromJust, fromMaybe, isJust, listToMaybe)
import Data.Word(Word32)
import Database.Esqueleto

import           BDCS.DB
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
createGroup fileIds tags = do
    -- Get the NEVRA so it can be saved as attributes
    let epoch = findTag "Epoch" tags >>= \t -> (tagValue t :: Maybe Word32) >>= Just . show
    let name = fromMaybe "" $ findStringTag "Name" tags
    let version = fromMaybe "" $ findStringTag "Version" tags
    let release = fromMaybe "" $ findStringTag "Release" tags
    let arch = fromMaybe "" $ findStringTag "Arch" tags

    -- Create the groups row
    let groupName = name ++ "-" ++ version ++ "-" ++ release ++ "." ++ arch
    groupId <- insert $ Groups groupName "rpm"

    -- Create the group_files rows
    void $ mapM (\fId -> insert $ GroupFiles groupId fId) fileIds

    -- Create the (E)NVRA attributes
    -- FIXME could at least deduplicate name and arch real easy
    forM_ [("name", name), ("version", version), ("release", release), ("arch", arch)] $ \(k, v) ->
        findKeyValue k v >>= \case
            Nothing -> insertKeyValue k v >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    -- Add the epoch attribute, when it exists.
    when (isJust epoch) $ void $ do
        let (k, v) = ("epoch", fromJust epoch)
        findKeyValue k v >>= \case
            Nothing -> insertKeyValue k v >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    -- Create the Provides attributes
    -- TODO How to handle everything from RPMSENSE_POSTTRANS and beyond?
    let provNames   = findStringListTag "ProvideName" tags
    let provFlags   = findWord32ListTag "ProvideFlags" tags
    let provVersion = findStringListTag  "ProvideVersion" tags

    forM_ (zip3 provNames provFlags provVersion) $ \(n, f, v) -> do
        let cmp  = rpmFlagsToOperator f
        let expr = dropWhileEnd isSpace $ n ++ " " ++ cmp ++ " " ++ v

        findKeyValue "rpm-provide" expr >>= \case
            Nothing -> insertKeyValue "rpm-provide" expr >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    -- Create the requirements
    -- TODO Conflicts, Obsoletes, Recommends, Enhances, Suggests, Supplements
    --      How to handle everything from RPMSENSE_POSTTRANS and beyond?
    let reqNames    = findStringListTag "RequireName" tags
    let reqFlags    = findWord32ListTag "RequireFlags" tags
    let reqVersions = findStringListTag "RequireVersion" tags

    forM_ (zip3 reqNames reqFlags reqVersions) $ \(n, f, v) -> do
        let cmp  = rpmFlagsToOperator f
        let expr = dropWhileEnd isSpace $ n ++ " " ++ cmp ++ " " ++ v

        reqId <- findRequires RT.RPM RT.Runtime RT.Must expr >>= \case
                     Nothing  -> insert $ Requirements RT.RPM RT.Runtime RT.Must expr
                     Just rid -> return rid

        void $ insert $ GroupRequirements groupId reqId

    return groupId

findRequires :: MonadIO m => RT.ReqLanguage -> RT.ReqContext -> RT.ReqStrength -> String -> SqlPersistT m (Maybe (Key Requirements))
findRequires reqLang reqCtx reqStrength reqExpr = do
    ndx <- select $ from $ \r -> do
           where_ (r ^. RequirementsReq_language ==. val reqLang &&.
                   r ^. RequirementsReq_context ==. val reqCtx &&.
                   r ^. RequirementsReq_strength ==. val reqStrength &&.
                   r ^. RequirementsReq_expr ==. val reqExpr)
           limit 1
           return (r ^. RequirementsId)
    return $ listToMaybe (map unValue ndx)
