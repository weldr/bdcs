{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.RPM.Scripts
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'Scripts' record support for RPM packages.

module BDCS.RPM.Scripts(mkScripts,
                        mkTriggerScripts)
 where

import Codec.RPM.Tags(Tag, findStringListTag, findTag, findWord32ListTag, tagValue)
import Data.List(zip6)
import Data.Maybe(catMaybes)
import Data.Text(pack)

import BDCS.DB(Scripts(..))

-- | Return a list of 'Scripts' records
mkScripts :: [Tag] -> [Scripts]
mkScripts tags = catMaybes [
    findTag "PreIn"  tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PreIn"  (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PostIn" tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PostIn" (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PreUn"  tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PreUn"  (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PostUn" tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PostUn" (pack body) Nothing Nothing Nothing Nothing Nothing,

    findTag "PreTrans" tags  >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PreTrans" (pack body) Nothing Nothing Nothing Nothing Nothing,
    findTag "PostTrans" tags >>= \t -> (tagValue t :: Maybe String) >>= \body -> Just $ Scripts "PostTrans" (pack body) Nothing Nothing Nothing Nothing Nothing
 ]

-- | Return a list of trigger scripts
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
