{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Utils.Conduit(awaitWith,
                     sourceInputStream)
 where

import Control.Monad(unless)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Resource(MonadResource)
import Data.ByteString(ByteString)
import Data.Conduit(Conduit, Producer, await, yield)
import Data.Maybe(fromMaybe)
import GI.Gio(IsInputStream, inputStreamReadBytes, noCancellable)
import GI.GLib(bytesGetData, bytesGetSize)

awaitWith :: Monad m => (i -> Conduit i m o) -> Conduit i m o
awaitWith fn = await >>= \case
    Nothing -> return ()
    Just v  -> fn v

-- Convert a GInputStream to a conduit source
sourceInputStream :: (MonadResource m, IsInputStream i) => i -> Producer m ByteString
sourceInputStream input = do
    let buf_size = 8096
    bytes <- liftIO $ inputStreamReadBytes input buf_size noCancellable
    bytesSize <- liftIO $ bytesGetSize bytes
    unless (bytesSize == 0) $ do
        bytesData <- liftIO $ bytesGetData bytes
        yield $ fromMaybe "" bytesData
        sourceInputStream input
