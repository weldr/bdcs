{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Sources
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Manage 'Sources' records in the database.  This record keeps track of a single
-- software release of a single project.  A single project can make many releases,
-- each of which will require a separate 'Sources' record.

module BDCS.Sources(findSource,
                    getSource,
                    insertSource,
                    insertSourceKeyValue)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import BDCS.KeyType
import BDCS.KeyValue(findKeyValue)

-- | Given a version number and a key to a 'Projects' record, find a matching software
-- source in the database.  If it exists, the database key is returned.
findSource :: MonadIO m => T.Text -> Key Projects -> SqlPersistT m (Maybe (Key Sources))
findSource version projectId = firstKeyResult $
    -- FIXME:  Is (project_id, version) unique in Sources?
    select $ from $ \src -> do
    where_ $ src ^. SourcesProject_id ==. val projectId &&.
             src ^. SourcesVersion    ==. val version
    limit 1
    return $ src ^. SourcesId

-- | Given a key to a 'Sources' record in the database, return that record.  This function
-- is suitable for using on the result of 'findSource'.
getSource :: MonadIO m => Key Sources -> SqlPersistT m (Maybe Sources)
getSource key = firstEntityResult $
    select $ from $ \source -> do
    where_ $ source ^. SourcesId ==. val key
    limit 1
    return source

-- | Conditionally add a new 'Sources' record to the database.  If the record already exists,
-- return its key.  Otherwise, insert the record and return the new key.
insertSource :: MonadIO m => Sources -> SqlPersistT m (Key Sources)
insertSource source@Sources{..} =
    findSource sourcesVersion sourcesProject_id `orInsert` source

-- | Conditionally add a new 'KeyVal' record to the database and associate a 'Sources'
-- record with it.  If the 'KeyVal' record already exists, it is reused in creating the
-- association.  They database key of the association is returned.
--
-- A single source can potentially have zero or more 'KeyVal' paris associated with it.
-- On the other hand, a single 'KeyVal' pair can apply to many sources.
insertSourceKeyValue :: MonadIO m =>
                        KeyType                             -- ^ Type of the 'KeyVal'
                     -> T.Text                              -- ^ Value of the 'KeyVal'
                     -> Maybe T.Text                        -- ^ Extended value of the 'KeyVal'
                     -> Key Sources                         -- ^ Source to be associated with the 'KeyVal'
                     -> SqlPersistT m (Key SourceKeyValues)
insertSourceKeyValue k v e sourceId = do
    kvId <- findKeyValue k (Just v) e `orInsert` KeyVal k (Just v) e
    insert $ SourceKeyValues sourceId kvId
