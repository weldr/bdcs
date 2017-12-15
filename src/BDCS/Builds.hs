{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Builds
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Manage 'Builds' records in the database.  This record keeps track of a single
-- build of a single software source (tarball, etc.).  A single source can be
-- built multiple times, each of which will require a separate 'Builds' record.

module BDCS.Builds(associateBuildWithPackage,
                   findBuild,
                   getBuild,
                   insertBuild,
                   insertBuildKeyValue)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import BDCS.KeyType
import BDCS.KeyValue(findKeyValue, insertKeyValue)

-- | Find a single build of a software package in the database, returning the database key
-- for that build if it exists.  All arguments are required and must be matched for this
-- function to return anything.  Note that conceptually, a build is of some software source
-- which is why a key to a 'Sources' record is required.
findBuild :: MonadIO m =>
             Int                                    -- ^ Epoch (usually, 0)
          -> T.Text                                 -- ^ Release
          -> T.Text                                 -- ^ Hardware architecture
          -> Key Sources                            -- ^ Reference to a 'Sources' record
          -> SqlPersistT m (Maybe (Key Builds))
findBuild epoch release arch sourceId = firstKeyResult $
    -- FIXME: Is (source_id, epoch, release, arch) unique in Builds?
    select $ from $ \build -> do
    where_ $ build ^. BuildsSource_id ==. val sourceId &&.
             build ^. BuildsEpoch ==. val epoch &&.
             build ^. BuildsRelease ==. val release &&.
             build ^. BuildsArch ==. val arch
    limit 1
    return $ build ^. BuildsId

-- | Given a key to a 'Builds' record in the database, return that record.  This function is
-- suitable for using on the result of 'findBuild'.
getBuild :: MonadIO m => Key Builds -> SqlPersistT m (Maybe Builds)
getBuild key = firstEntityResult $
    select $ from $ \build -> do
    where_ $ build ^. BuildsId ==. val key
    limit 1
    return build

-- | Conditionally add a new 'Builds' record to the database.  If the record already exists,
-- return its key.  Otherwise, insert the record and return the new key.
insertBuild :: MonadIO m => Builds -> SqlPersistT m (Key Builds)
insertBuild build@Builds{..} =
    findBuild buildsEpoch buildsRelease buildsArch buildsSource_id `orInsert` build

-- | Conditionally add a new 'KeyVal' record to the database and associate a 'Builds' record
-- with it.  If the 'KeyVal' record already exists, it is reused in creating the association.
-- The database key of the association is returned.
insertBuildKeyValue :: MonadIO m =>
                       KeyType                              -- ^ Type of the 'KeyVal'
                    -> T.Text                               -- ^ Value of the 'KeyVal'
                    -> Maybe T.Text                         -- ^ Extended value of the 'KeyVal'
                    -> Key Builds                           -- ^ Build to be associated with the 'KeyVal'
                    -> SqlPersistT m (Key BuildKeyValues)
insertBuildKeyValue k v e buildId =
    maybeKey (insertKeyValue k (Just v) e >>= associateBuildWithPackage buildId)
             (associateBuildWithPackage buildId)
             (findKeyValue k (Just v) e)

-- | Create a link in the database between an existing 'Builds' record and an existing 'KeyVal'
-- record.  This is different from 'insertBuildKeyValue', which also creates the 'KeyVal' record.
-- A single build can potentially have zero or many 'KeyVal' pairs associated with it.  On the
-- other hand, a single 'KeyVal' pair can apply to many builds.
--
-- The database key of the new link is returned.
associateBuildWithPackage :: MonadIO m => Key Builds -> Key KeyVal -> SqlPersistT m (Key BuildKeyValues)
associateBuildWithPackage buildId kvId =
    insert $ BuildKeyValues buildId kvId
