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

module BDCS.Files(insertFiles,
                  associateFilesWithBuild,
                  associateFilesWithPackage,
                  groupIdToFiles,
                  groupIdToFilesC)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Trans.Resource(MonadResource)
import           Data.Conduit((.|), Conduit, Source, await, toProducer)
import qualified Data.Conduit.List as CL
import           Database.Esqueleto

import BDCS.DB

insertFiles :: MonadIO m => [Files] -> SqlPersistT m [Key Files]
insertFiles = mapM insert

associateFilesWithBuild :: MonadIO m => [Key Files] -> Key Builds -> SqlPersistT m [Key BuildFiles]
associateFilesWithBuild files build =
    mapM (\(fID, bID) -> insert $ BuildFiles bID fID)
         (zip files $ repeat build)

associateFilesWithPackage :: MonadIO m => [Key Files] -> Key KeyVal -> SqlPersistT m [Key FileKeyValues]
associateFilesWithPackage files package =
    mapM (\(fID, pID) -> insert $ FileKeyValues fID pID)
         (zip files $ repeat package)

groupIdToFiles :: MonadResource m => Key Groups -> Source (SqlPersistT m) Files
groupIdToFiles groupid = do
    let source = selectSource $ from $ \(files `InnerJoin` group_files) -> do
                       on     $ files ^. FilesId ==. group_files ^. GroupFilesFile_id
                       where_ $ group_files ^. GroupFilesGroup_id ==. val groupid
                       return files
    source .| CL.map entityVal

groupIdToFilesC :: MonadResource m => Conduit (Key Groups) (SqlPersistT m) Files
groupIdToFilesC = await >>= \case
    Nothing      -> return ()
    Just groupid -> toProducer (groupIdToFiles groupid) >> groupIdToFilesC
