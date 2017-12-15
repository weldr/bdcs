{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: BDCS.ReqType
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Data types for working with 'Requirements'.

module BDCS.ReqType(ReqContext(..),
                    ReqLanguage(..),
                    ReqStrength(..))
 where

import Database.Persist.TH

{-# ANN module "HLint: ignore Use module export list" #-}

-- | The type of a requirements language - this basically maps to a packaging
-- system.  For now, only RPM is supported.
data ReqLanguage = RPM
 deriving(Eq, Read, Show)

-- | The type for specifying when a requirement should be enforced.
data ReqContext = Build                 -- ^ Applies when building
                | Runtime               -- ^ Applies when the package is on the system
                | Test                  -- ^ Applies to running tests
                | ScriptPre             -- ^ Before a package install
                | ScriptPost            -- ^ After a package install
                | ScriptPreUn           -- ^ Before a package uninstall
                | ScriptPostUn          -- ^ After a package uninstall
                | ScriptPreTrans        -- ^ Before a package transaction
                | ScriptPostTrans       -- ^ After a package transaction
                | ScriptVerify          -- ^ Package verification script
                | Feature               -- ^ Feature requirement, e.g. rpmlib features
 deriving(Eq, Read, Show)

-- | The type for specifying how strictly a requirement should be enforced.
data ReqStrength = Must                 -- ^ A requirement must be satisfied
                 | Should               -- ^ A requirement should be satisfied, but it is
                                        --   not an error if it cannot be.
                 | May                  -- ^ A requirement should only be satisfied at the
                                        --   user's option.  Typically, automated processes
                                        --   will ignore this.
                 | ShouldIfInstalled    -- ^ Like 'Should', but looks at packages that are
                                        --   already installed.
                 | MayIfInstalled       -- ^ Like 'May', but looks at packages that are
                                        --   already installed.
 deriving(Eq, Read, Show)

derivePersistField "ReqLanguage"
derivePersistField "ReqContext"
derivePersistField "ReqStrength"
