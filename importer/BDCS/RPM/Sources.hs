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

module BDCS.RPM.Sources(mkSource)
 where

import Database.Esqueleto(Key)

import BDCS.DB(Projects, Sources(..))
import BDCS.Exceptions(DBException(..), throwIfNothing)
import RPM.Tags(Tag, findStringTag)

mkSource :: [Tag] -> Key Projects -> Sources
mkSource tags projectId = let
    license = findStringTag "License" tags `throwIfNothing` MissingRPMTag "License"
    version = findStringTag "Version" tags `throwIfNothing` MissingRPMTag "Version"

    -- FIXME:  Where to get this from?
    source_ref = "SOURCE_REF"
 in
    Sources projectId license version source_ref
