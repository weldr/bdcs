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

module BDCS.Label.License(matches,
                          mkLabel)
 where

import           Data.Char(toUpper)
import           Data.List(isPrefixOf)
import qualified Data.Text as T
import           System.FilePath.Posix(takeFileName)

import BDCS.DB(Files(..))
import BDCS.Label.Types(Label(..))

feq :: FilePath -> String -> Bool
feq path s = let
    path' = map toUpper path
    sDot  = s ++ "."
    sDash = s ++ "-"
 in
    path' == s ||
    sDot `isPrefixOf` path ||
    sDash `isPrefixOf` path

matches :: Files -> Bool
matches Files{..} = let
    filesPath' = T.unpack filesPath
    fn         = takeFileName filesPath'
 in
    "/usr/share/licenses/" `isPrefixOf` filesPath' ||
    feq fn "COPYING" || feq fn "COPYRIGHT" || feq fn "LICENSE"

mkLabel :: Files -> Maybe Label
mkLabel _ = Just LicenseLabel
