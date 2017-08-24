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

module BDCS.Files(insertFiles,
                  associateFilesWithBuild,
                  associateFilesWithPackage,
                  files,
                  filesC,
                  groupIdToFiles,
                  groupIdToFilesC,
                  keyValsForFile,
                  pathToGroupId)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Trans.Resource(MonadResource)
import           Data.Conduit((.|), Conduit, Source, toProducer)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB
import Utils.Conduit(awaitWith)

insertFiles :: MonadIO m => [Files] -> SqlPersistT m [Key Files]
insertFiles = mapM insert

associateFilesWithBuild :: MonadIO m => [Key Files] -> Key Builds -> SqlPersistT m [Key BuildFiles]
associateFilesWithBuild fs build =
    mapM (\(fID, bID) -> insert $ BuildFiles bID fID)
         (zip fs $ repeat build)

associateFilesWithPackage :: MonadIO m => [Key Files] -> Key KeyVal -> SqlPersistT m [Key FileKeyValues]
associateFilesWithPackage fs package =
    mapM (\(fID, pID) -> insert $ FileKeyValues fID pID)
         (zip fs $ repeat package)

files :: MonadIO m => SqlPersistT m [Files]
files = do
    results <- select  $ from $ \file -> do
               orderBy [asc (file ^. FilesPath)]
               return  file
    return $ map entityVal results

filesC :: (MonadResource m, MonadIO m) => Source (SqlPersistT m) Files
filesC = do
    let source = selectSource $ from $ \file -> do
                 orderBy      [asc (file ^. FilesPath)]
                 return       file
    source .| CL.map entityVal

groupIdToFiles :: MonadResource m => Key Groups -> Source (SqlPersistT m) Files
groupIdToFiles groupid = do
    let source = selectSource $ from $ \(fs `InnerJoin` group_files) -> do
                       on     $ fs ^. FilesId ==. group_files ^. GroupFilesFile_id
                       where_ $ group_files ^. GroupFilesGroup_id ==. val groupid
                       return fs
    source .| CL.map entityVal

groupIdToFilesC :: MonadResource m => Conduit (Key Groups) (SqlPersistT m) Files
groupIdToFilesC = awaitWith $ \groupid -> toProducer (groupIdToFiles groupid) >> groupIdToFilesC

keyValsForFile :: MonadIO m => T.Text -> SqlPersistT m [KeyVal]
keyValsForFile path = do
    results <- select $ from $ \(file `InnerJoin` file_key_val `InnerJoin` key_val) -> do
               on     $ file ^. FilesId ==. file_key_val ^. FileKeyValuesFile_id &&.
                        key_val ^. KeyValId ==. file_key_val ^. FileKeyValuesKey_val_id
               where_ $ file ^. FilesPath ==. val path
               return key_val
    return $ map entityVal results

pathToGroupId :: MonadIO m => T.Text -> SqlPersistT m [Key Groups]
pathToGroupId path = do
    vals <- select $ distinct $ from $ \(group_files `InnerJoin` fs) -> do
            on     $ group_files ^. GroupFilesFile_id ==. fs ^. FilesId
            where_ $ fs ^. FilesPath ==. val path
            return $ group_files ^. GroupFilesGroup_id
    return $ map unValue vals
