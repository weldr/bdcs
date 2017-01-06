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

{-# LANGUAGE LambdaCase #-}

module BDCS.Sources(findSource,
                    insertSource,
                    mkSource)
 where

import Control.Monad.IO.Class(MonadIO)
import Data.Maybe(listToMaybe)
import Database.Esqueleto

import BDCS.DB
import BDCS.Exceptions(DBException(..), throwIfNothing, throwIfNothingOtherwise)
import RPM.Tags(Tag, findStringTag)

findSource :: MonadIO m => String -> Key Projects -> SqlPersistT m (Maybe (Key Sources))
findSource version projectId = do
    -- FIXME:  Is (project_id, version) unique in Sources?
    ndx <- select $ from $ \src -> do
           where_ (src ^. SourcesProject_id ==. val projectId &&.
                   src ^. SourcesVersion    ==. val version)
           limit 1
           return (src ^. SourcesId)
    return $ listToMaybe (map unValue ndx)

insertSource :: MonadIO m => [Tag] -> Key Projects -> SqlPersistT m (Key Sources)
insertSource rpm projectId =
    throwIfNothingOtherwise sourceVersion (DBException "No Version tag") $ \v ->
        findSource v projectId >>= \case
            Nothing  -> insert $ mkSource rpm projectId `throwIfNothing` DBException "Couldn't make Sources record"
            Just src -> return src
 where
    sourceVersion = findStringTag "Version" rpm

mkSource :: [Tag] -> Key Projects -> Maybe Sources
mkSource tags projectId = do
    license <- findStringTag "License" tags
    version <- sourceVersion

    -- FIXME:  Where to get this from?
    let source_ref = "SOURCE_REF"

    return $ Sources projectId license version source_ref
 where
    sourceVersion = findStringTag "Version" tags
