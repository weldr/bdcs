{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Scripts
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Manage 'Scripts' records in the database.  This record keeps track of a single
-- script that is associated with a 'Groups' record.  A script is some arbitrary
-- program that can be run at various points when installing, upgrading, or removing
-- a piece of software.  Not all packaging systems support this concept, and the
-- BDCS only somewhat supports them at the moment.

module BDCS.Scripts(findScript,
                    getScript,
                    insertScript)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB

-- | Find a single script in the database, returning the key for that script if it
-- exists.  It is possible for multiple very similar scripts to exist in the same
-- database (or even, scripts with identical bodies but that differ in other ways)
-- so additional information must be provided.
findScript :: MonadIO m =>
              T.Text                                -- ^ The script type (generally, when it runs).
                                                    -- This value is highly packaging system dependent.
           -> T.Text                                -- ^ The script body
           -> Maybe T.Text                          -- ^ The name of any trigger required to fire
                                                    -- off this script.  Most scripts do not use this,
                                                    -- and many packaging systems do not support it.
           -> Maybe T.Text                          -- ^ The script version, currently unused
           -> Maybe Int                             -- ^ The script index, currently unused
           -> Maybe Int                             -- ^ The script flags, currently unused
           -> SqlPersistT m (Maybe (Key Scripts))
findScript ty body name _ver _ndx _flags = firstKeyResult $
    select $ from $ \script -> do
    where_ $ script ^. ScriptsTy ==. val ty &&.
             script ^. ScriptsBody ==. val body &&.
             script ^. ScriptsTrigger_name ==? name
    limit 1
    return $ script ^. ScriptsId

-- | Given a key to a 'Scripts' record in the database, return that record.  This function is
-- suitable for using on the result of 'findScript'.
getScript :: MonadIO m => Key Scripts -> SqlPersistT m (Maybe Scripts)
getScript key = firstEntityResult $
    select $ from $ \script -> do
    where_ $ script ^. ScriptsId ==. val key
    limit 1
    return script

-- | Conditionally add a new 'Scripts' record to the database and associate a 'Groups' record
-- with it.  If the association already exists, it is reused in creating the association.
-- The database key of the association is returned.  A single group can potentially have zero
-- or many 'Scripts' associated with it.
insertScript :: MonadIO m => Key Groups -> Scripts -> SqlPersistT m (Key GroupScripts)
insertScript groupId script@Scripts{..} =
    maybeKey (insert script >>= insert . GroupScripts groupId)
             (insert . GroupScripts groupId)
             (findScript scriptsTy scriptsBody scriptsTrigger_name scriptsTrigger_version scriptsTrigger_index scriptsTrigger_flags)
