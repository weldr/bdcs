-- Copyright (C) 2016 Red Hat, Inc.
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

module BDCS.Projects(findProject,
                     getProject,
                     insertProject,
                     projects)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB

projects :: MonadIO m => SqlPersistT m [Projects]
projects = do
    vals <- select $ from $ \project -> do
            orderBy [asc (project ^. ProjectsName)]
            return project
    return $ map entityVal vals

findProject :: MonadIO m => T.Text -> SqlPersistT m (Maybe (Key Projects))
findProject name = firstKeyResult $
    select $ from $ \proj -> do
    where_ $ proj ^. ProjectsName ==. val name
    limit 1
    return $ proj ^. ProjectsId

getProject :: MonadIO m => Key Projects -> SqlPersistT m (Maybe Projects)
getProject key = firstEntityResult $
    select $ from $ \proj -> do
    where_ $ proj ^. ProjectsId ==. val key
    limit 1
    return proj

insertProject :: MonadIO m => Projects -> SqlPersistT m (Key Projects)
insertProject project@Projects{..} =
    findProject projectsName `orInsert` project
