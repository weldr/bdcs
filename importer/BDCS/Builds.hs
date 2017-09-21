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

{-# LANGUAGE RecordWildCards #-}

module BDCS.Builds(associateBuildWithPackage,
                   findBuild,
                   getBuild,
                   insertBuild)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB

findBuild :: MonadIO m => Int -> T.Text -> T.Text -> Key Sources -> SqlPersistT m (Maybe (Key Builds))
findBuild epoch release arch sourceId = firstKeyResult $
    -- FIXME: Is (source_id, epoch, release, arch) unique in Builds?
    select $ from $ \build -> do
    where_ $ build ^. BuildsSource_id ==. val sourceId &&.
             build ^. BuildsEpoch ==. val epoch &&.
             build ^. BuildsRelease ==. val release &&.
             build ^. BuildsArch ==. val arch
    limit 1
    return $ build ^. BuildsId

getBuild :: MonadIO m => Key Builds -> SqlPersistT m (Maybe Builds)
getBuild key = firstEntityResult $
    select $ from $ \build -> do
    where_ $ build ^. BuildsId ==. val key
    limit 1
    return build

insertBuild :: MonadIO m => Builds -> SqlPersistT m (Key Builds)
insertBuild build@Builds{..} =
    findBuild buildsEpoch buildsRelease buildsArch buildsSource_id `orInsert` build

associateBuildWithPackage :: MonadIO m => Key Builds -> Key KeyVal -> SqlPersistT m (Key BuildKeyValues)
associateBuildWithPackage buildId kvId =
    insert $ BuildKeyValues buildId kvId
