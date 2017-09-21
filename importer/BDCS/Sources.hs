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

module BDCS.Sources(findSource,
                    getSource,
                    insertSource)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB

findSource :: MonadIO m => T.Text -> Key Projects -> SqlPersistT m (Maybe (Key Sources))
findSource version projectId = firstKeyResult $
    -- FIXME:  Is (project_id, version) unique in Sources?
    select $ from $ \src -> do
    where_ $ src ^. SourcesProject_id ==. val projectId &&.
             src ^. SourcesVersion    ==. val version
    limit 1
    return $ src ^. SourcesId

getSource :: MonadIO m => Key Sources -> SqlPersistT m (Maybe Sources)
getSource key = firstEntityResult $
    select $ from $ \source -> do
    where_ $ source ^. SourcesId ==. val key
    limit 1
    return source

insertSource :: MonadIO m => Sources -> SqlPersistT m (Key Sources)
insertSource source@Sources{..} =
    findSource sourcesVersion sourcesProject_id `orInsert` source
