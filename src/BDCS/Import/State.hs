-- |
-- Module: BDCS.Import.State
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Types needed to maintain state when importing objects.

module BDCS.Import.State(ImportState(..))
 where

import Data.ContentStore(ContentStore)

-- | The state record.  This is helpful for keeping track of various pieces of
-- global data when importing objects.
data ImportState = ImportState { stDB :: FilePath,          -- ^ The metadata database
                                 stRepo :: ContentStore     -- ^ The opened, initialized content store
                               }
