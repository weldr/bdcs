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

module BDCS.RPM.Requirements(mkGroupRequirement,
                             mkRequirement)
 where

import           Database.Esqueleto(Key)
import qualified Data.Text as T

import           BDCS.DB(GroupRequirements(..), Groups(..), Requirements(..))
import qualified BDCS.ReqType as RT

{-# ANN module "HLint: ignore Eta reduce" #-}

mkGroupRequirement :: Key Groups -> Key Requirements -> GroupRequirements
mkGroupRequirement groupId reqId =
    GroupRequirements groupId reqId

mkRequirement :: RT.ReqStrength -> T.Text -> Requirements
mkRequirement strength expr =
    Requirements RT.RPM RT.Runtime strength expr
