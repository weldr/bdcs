{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: BDCS.Utils.Conduit
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Conduit related utility functions

module BDCS.Utils.Conduit(awaitWith,
                          identityC,
                          sourceInputStream)
 where

import Conduit(mapC)
import Control.Monad(unless)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Resource(MonadResource)
import Data.ByteString(ByteString)
import Data.Conduit(Conduit, Producer, await, yield)
import Data.Maybe(fromMaybe)
import GI.Gio(IsInputStream, inputStreamReadBytes, noCancellable)
import GI.GLib(bytesGetData, bytesGetSize)

-- | Wait for a single value and then call fn on it.
awaitWith :: Monad m => (i -> Conduit i m o) -> Conduit i m o
awaitWith fn = await >>= \case
    Nothing -> return ()
    Just v  -> fn v

-- | A conduit that takes its input and returns that as its output.
identityC :: Monad m => Conduit a m a
identityC = mapC id

-- | Convert a GInputStream to a conduit source
sourceInputStream :: (MonadResource m, IsInputStream i) => i -> Producer m ByteString
sourceInputStream input = do
    let buf_size = 8096
    bytes <- liftIO $ inputStreamReadBytes input buf_size noCancellable
    bytesSize <- liftIO $ bytesGetSize bytes
    unless (bytesSize == 0) $ do
        bytesData <- liftIO $ bytesGetData bytes
        yield $ fromMaybe "" bytesData
        sourceInputStream input
