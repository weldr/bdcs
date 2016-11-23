module BDCS.KeyValue(findKeyValue,
                     insertKeyValue)
 where

import Control.Monad.IO.Class(MonadIO)
import Database.Esqueleto
import Data.Maybe(listToMaybe)

import BDCS.DB

findKeyValue :: MonadIO m => String -> String -> SqlPersistT m (Maybe (Key KeyVal))
findKeyValue k v = do
    ndx <- select $ from $ \kv -> do
           where_ (kv ^. KeyValKey_value ==. val k &&.
                   kv ^. KeyValVal_value ==. val v)
           limit 1
           return (kv ^. KeyValId)
    return $ listToMaybe (map unValue ndx)

insertKeyValue :: MonadIO m => String -> String -> SqlPersistT m (Key KeyVal)
insertKeyValue k v =
    insert (KeyVal k v)
