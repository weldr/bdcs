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

{-# LANGUAGE OverloadedStrings #-}

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
