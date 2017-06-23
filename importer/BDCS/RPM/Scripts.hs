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

{-# LANGUAGE OverloadedStrings #-}

module BDCS.RPM.Scripts(mkScripts,
                        mkTriggerScripts)
 where

import Data.List(zip6)
import Data.Maybe(catMaybes)
import Data.Text(pack)
import Database.Esqueleto

import BDCS.DB(Scripts(..))
import BDCS.Exceptions(DBException(..), throwIfNothing)
import RPM.Tags(Tag, findStringListTag, findTag, findWord32ListTag, tagValue)

mkScripts :: [Tag] -> [Scripts]
mkScripts tags = catMaybes [
    findTag "PreIn"  tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PreIn"  (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PostIn" tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PostIn" (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PreUn"  tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PreUn"  (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PostUn" tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PostUn" (pack body) Nothing Nothing Nothing Nothing Nothing,

    findTag "PreTrans" tags  >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PreTrans" (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PostTrans" tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PostTrans" (pack body) Nothing Nothing Nothing Nothing Nothing
 ]

mkTriggerScripts :: [Tag] -> [Scripts]
mkTriggerScripts tags = let
    bodies = map pack         $ findStringListTag "TriggerScripts"    tags
    names  = map pack         $ findStringListTag "TriggerName"       tags
    vers   = map pack         $ findStringListTag "TriggerVersion"    tags
    flags  = map fromIntegral $ findWord32ListTag "TriggerFlags"      tags
    ndxs   = map fromIntegral $ findWord32ListTag "TriggerIndex"      tags
    progs  = map pack         $ findStringListTag "TriggerScriptProg" tags
 in
    map (\(b, n, v, f, x, p) -> Scripts "Trigger" b (Just p) (Just x) (Just n) (Just v) (Just f))
        (zip6 bodies names vers flags ndxs progs)
