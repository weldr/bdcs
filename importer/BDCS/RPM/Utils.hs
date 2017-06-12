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

module BDCS.RPM.Utils(splitFilename)
 where

import           Data.Bifunctor(first)
import qualified Data.Text as T

-- Turn an RPM filename in form of "NAME-[EPOCH:]VERSION-RELEASE.ARCH[.rpm]
-- into a tuple of (name, epoch, version, release, and arch).
splitFilename :: T.Text -> (T.Text, Maybe T.Text, T.Text, T.Text, T.Text)
splitFilename rpm_ = let
    rpm = if ".rpm" `T.isSuffixOf` rpm_ then T.dropEnd 4 rpm_ else rpm_

    (front,  a) = T.breakOnEnd "." rpm
    (front2, r) = T.breakOnEnd "-" $ T.dropWhileEnd (== '.') front
    (n,     ve) = first (T.dropWhileEnd (== '-')) $ T.breakOnEnd "-" $ T.dropWhileEnd (== '-') front2
    (e,      v) = first (T.dropWhileEnd (== ':')) $ T.breakOnEnd ":" ve
 in
    (n, if e == "" then Nothing else Just $ T.dropWhile (== ':') e, v, r, a)
