{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.RPM.Builds
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'Builds' record support for RPM packages.

module BDCS.RPM.Builds(mkBuild)
 where

import           Codec.RPM.Tags(Tag, findStringTag, findStringListTag, findTag, tagValue)
import           Data.ByteString.Char8(pack)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Data.Word(Word32)
import           Database.Esqueleto(Key)

import BDCS.DB(Builds(..), Sources)
import BDCS.Exceptions(DBException(..), throwIfNothing)

-- | Return a 'Builds' record for the RPM package.
--
-- Can throw MissingRPMTag
mkBuild :: [Tag] -> Key Sources -> Builds
mkBuild tags sourceId = let
    epoch   = maybe 0 fromIntegral (findTag "Epoch" tags >>= \t -> tagValue t :: Maybe Word32)
    release = T.pack $ findStringTag "Release" tags `throwIfNothing` MissingRPMTag "Release"
    arch    = T.pack $ findStringTag "Arch"    tags `throwIfNothing` MissingRPMTag "Arch"

    build_time = getBuildTime `throwIfNothing` MissingRPMTag "BuildTime"
    -- FIXME: RPM splits the changelog up into three tag types.  I'm just grabbing the text here for now.
    changelog  = getChangelog `throwIfNothing` MissingRPMTag "ChangeLogText"

    -- FIXME:  Where to get these from?
    build_config_ref = "BUILD_CONFIG_REF"
    build_env_ref = "BUILD_ENV_REF"
 in
    Builds sourceId epoch release arch build_time changelog build_config_ref build_env_ref
 where
    getBuildTime = findTag "BuildTime" tags >>= \t -> (tagValue t :: Maybe Word32) >>= Just . posixSecondsToUTCTime . realToFrac

    getChangelog = case findStringListTag "ChangeLogText" tags of
                       []  -> Nothing
                       lst -> Just $ pack $ head lst
