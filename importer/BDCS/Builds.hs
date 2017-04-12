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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module BDCS.Builds(associateBuildWithPackage,
                   findBuild,
                   insertBuild)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Data.Maybe(listToMaybe)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB

findBuild :: MonadIO m => Int -> T.Text -> T.Text -> Key Sources -> SqlPersistT m (Maybe (Key Builds))
findBuild epoch release arch sourceId = do
    -- FIXME: Is (source_id, epoch, release, arch) unique in Builds?
    ndx <- select $ from $ \build -> do
           where_ $ build ^. BuildsSource_id ==. val sourceId &&.
                    build ^. BuildsEpoch ==. val epoch &&.
                    build ^. BuildsRelease ==. val release &&.
                    build ^. BuildsArch ==. val arch
           limit 1
           return $ build ^. BuildsId
    return $ listToMaybe (map unValue ndx)

insertBuild :: MonadIO m => Builds -> SqlPersistT m (Key Builds)
insertBuild build@Builds{..} =
    findBuild buildsEpoch buildsRelease buildsArch buildsSource_id >>= \case
        Nothing  -> insert build
        Just bld -> return bld

associateBuildWithPackage :: MonadIO m => Key Builds -> Key KeyVal -> SqlPersistT m (Key BuildKeyValues)
associateBuildWithPackage buildId kvId =
    insert $ BuildKeyValues buildId kvId
