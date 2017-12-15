-- |
-- Module: BDCS.Signatures
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Manage 'BuildSignatures' records in the database.  These are used for keeping track
-- of the signatures given to a build at build time.  Multiple signatures can be associated
-- with a single 'Builds' record.

module BDCS.Signatures(insertBuildSignatures)
 where

import Control.Monad.IO.Class(MonadIO)
import Database.Esqueleto

import BDCS.DB

-- | Loop over a list of provided 'BuildSignatures' and add them to the database, returning
-- their keys in the same order as the original list.
insertBuildSignatures :: MonadIO m => [BuildSignatures] -> SqlPersistT m [Key BuildSignatures]
insertBuildSignatures = mapM insert
