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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BDCS.KeyValue(findKeyValue,
                     formatKeyValue,
                     getKeyValue,
                     insertKeyValue,
                     keyValueListToJSON)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Data.Aeson((.=), object, toJSON)
import           Data.Aeson.Types(KeyValue)
import           Data.List(partition)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import BDCS.KeyType

findKeyValue :: MonadIO m => KeyType -> Maybe T.Text -> Maybe T.Text -> SqlPersistT m (Maybe (Key KeyVal))
findKeyValue k v e = firstKeyResult $
    select $ from $ \kv -> do
    where_ $ kv ^. KeyValKey_value ==. val k &&.
             kv ^. KeyValVal_value ==? v &&.
             kv ^. KeyValExt_value ==? e
    limit 1
    return $ kv ^. KeyValId

formatKeyValue :: KeyVal -> T.Text
formatKeyValue KeyVal{..} = let
    rhs = case (keyValVal_value, keyValExt_value) of
              (Just v, Nothing) -> T.concat [ " = ", v ]
              (Just v, Just e)  -> T.concat [ " = (", v, ", ", e, ")" ]
              _                 -> ""
 in
    T.concat [ T.pack $ show keyValKey_value, rhs ]

getKeyValue :: MonadIO m => Key KeyVal -> SqlPersistT m (Maybe KeyVal)
getKeyValue key = firstEntityResult $
    select $ from $ \kv -> do
    where_ $ kv ^. KeyValId ==. val key
    limit 1
    return kv

insertKeyValue :: MonadIO m => KeyType -> Maybe T.Text -> Maybe T.Text -> SqlPersistT m (Key KeyVal)
insertKeyValue k v e =
    insert (KeyVal k v e)

keyValueListToJSON :: KeyValue t => [KeyVal] -> [t]
keyValueListToJSON lst = let
    isLabelKey (LabelKey _) = True
    isLabelKey _            = False

    -- We want to handle LabelKeys differently from all other KeyTypes, so first let's sort them out.
    (labelKvs, otherKvs) = partition (isLabelKey . keyValKey_value) lst

    -- Convert LabelKeys into tuples of ("labels", json).  All LabelKeys will have the same first value
    -- in their tuple - the string "labels".  This assumes that LabelKeys do not store values or extended
    -- values.  If they start doing that, this will have to change.
    labelVals = map (\kv -> (T.pack "labels", [toJSON $ keyValKey_value kv])) labelKvs

    -- Convert all other KeyTypes into tuples of (key, json).
    otherVals = map (\kv -> (asText $ keyValKey_value kv, [toJSON kv])) otherKvs

    -- A single list can have many KeyVals with the same key (think about rpm-provides and requires
    -- especially).  We use an intermediate map to turn it into a list of (key, [json1, json2, ...]) tuples.
    -- Both types get handled the same way here.
    labelMap = Map.fromListWith (++) labelVals
    otherMap = Map.fromListWith (++) otherVals

    -- If there's only one KeyVal for a given key, strip the list out before converting it to a
    -- json list object.  Otherwise, everything will end up in a list.
    --
    -- On the other hand, we don't do anything to LabelKeys.  This means labels will always end up
    -- in a list named "labels".
    pairs = map (\(k, v) -> case v of
                                [hd] -> k .= hd
                                _    -> k .= v)
                (Map.toList otherMap) ++
            map (uncurry (.=)) (Map.toList labelMap)
 in
    [T.pack "keyvals" .= object pairs]
