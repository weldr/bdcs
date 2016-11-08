module BDCS.KeyValue(insertKeyValue)
 where

import Control.Monad.IO.Class(MonadIO)
import Database.Esqueleto

import BDCS.DB

insertKeyValue :: MonadIO m => String -> String -> SqlPersistT m (Key KeyVal)
insertKeyValue k v =
    insert (KeyVal k v)
