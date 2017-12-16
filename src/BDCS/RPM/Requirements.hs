-- |
-- Module: BDCS.RPM.Requirements
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'Requirements' record support for RPM packages.

module BDCS.RPM.Requirements(mkGroupRequirement,
                             mkRequirement)
 where

import           Database.Esqueleto(Key)
import qualified Data.Text as T

import           BDCS.DB(GroupRequirements(..), Groups(..), Requirements(..))
import qualified BDCS.ReqType as RT

{-# ANN module "HLint: ignore Eta reduce" #-}

-- | Return a 'GroupRequirements' record for the RPM package.
mkGroupRequirement :: Key Groups -> Key Requirements -> GroupRequirements
mkGroupRequirement groupId reqId =
    GroupRequirements groupId reqId

-- | Return a 'Requirements' record for the RPM package.
mkRequirement :: RT.ReqContext -> RT.ReqStrength -> T.Text -> Requirements
mkRequirement context strength expr =
    Requirements RT.RPM context strength expr
