{-# LANGUAGE LambdaCase #-}

module BDCS.Packages(filesInPackage,
                     findPackage,
                     insertPackageName)
 where

import Control.Monad.IO.Class(MonadIO)
import Data.Maybe(listToMaybe)
import Database.Esqueleto

import BDCS.DB
import BDCS.Exceptions(DBException(..), throwIfNothingOtherwise)
import BDCS.KeyValue(insertKeyValue)
import RPM.Tags(Tag, findStringTag)

filesInPackage :: MonadIO m => String -> SqlPersistT m [String]
filesInPackage name = do
    results <- select $ from $ \(files `InnerJoin` key_val `InnerJoin` file_key_values) -> do
               on (key_val ^. KeyValId ==. file_key_values ^. FileKeyValuesKey_val_id &&.
                   file_key_values ^. FileKeyValuesFile_id ==. files ^. FilesId)
               where_ (key_val ^. KeyValKey_value ==. val "packageName" &&.
                       key_val ^. KeyValVal_value ==. val name)
               return (files ^. FilesPath)
    return $ map unValue results

insertPackageName :: MonadIO m => [Tag] -> SqlPersistT m (Key KeyVal)
insertPackageName rpm =
    throwIfNothingOtherwise packageName (DBException "No Name tag") $ \name ->
        findPackage name >>= \case
            Nothing -> insertKeyValue "packageName" name
            Just p  -> return p
 where
    packageName = findStringTag "Name" rpm

findPackage :: MonadIO m => String -> SqlPersistT m (Maybe (Key KeyVal))
findPackage name = do
    ndx <- select $ from $ \pkg -> do
           where_ (pkg ^. KeyValKey_value ==. val "packageName" &&.
                   pkg ^. KeyValVal_value ==. val name)
           limit 1
           return (pkg ^. KeyValId)
    return $ listToMaybe (map unValue ndx)
