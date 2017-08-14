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

{-# LANGUAGE TemplateHaskell #-}

module BDCS.ReqType where

import Database.Persist.TH

{-# ANN module "HLint: ignore Use module export list" #-}

data ReqLanguage = RPM
 deriving(Eq, Read, Show)

data ReqContext = Build
                | Runtime
                | Test
                | ScriptPre         -- before a package install
                | ScriptPost        -- after a package install
                | ScriptPreUn       -- before a package uninstall
                | ScriptPostUn      -- after a package uninstall
                | ScriptPreTrans    -- before a package transaction
                | ScriptPostTrans   -- after a package transaction
                | ScriptVerify      -- package verification script
                | Feature           -- feature requirement, e.g. rpmlib features
 deriving(Eq, Read, Show)

data ReqStrength = Must | Should | May | ShouldIfInstalled | MayIfInstalled
 deriving(Eq, Read, Show)

derivePersistField "ReqLanguage"
derivePersistField "ReqContext"
derivePersistField "ReqStrength"
