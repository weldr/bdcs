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

{-# LANGUAGE RecordWildCards #-}

module BDCS.Label.InfoPage(matches,
                           mkLabel)
 where

import           Data.List(isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import           Text.Regex.PCRE((=~))

import BDCS.DB(Files(..))
import BDCS.Label.Types(Label(..))

matches :: Files -> Bool
matches Files{..} = let
    filesPath' = T.unpack filesPath
 in
    "/usr/share/info/" `isPrefixOf` filesPath' &&
    (".info.gz" `isSuffixOf` filesPath' || filesPath' =~ "\\.info-[0-9]+\\.gz")

mkLabel :: Files -> Label
mkLabel _ = InfoPageLabel
