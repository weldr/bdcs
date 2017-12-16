-- |
-- Module: BDCS.Utils.Error
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions to help with errors

module BDCS.Utils.Error(errorToEither,
                        errorToMaybe,
                        mapError)

 where

import Control.Monad.Except(ExceptT(..), MonadError, catchError, runExceptT, throwError)

-- | Convert an error action into an Either
-- This is essentially runExceptT generalized to MonadError
errorToEither :: MonadError e m => m a -> m (Either e a)
errorToEither action = (Right <$> action) `catchError` (return . Left)

-- | Convert an error into into nothing
errorToMaybe :: MonadError e m => m a -> m (Maybe a)
errorToMaybe action = (Just <$> action) `catchError` (const . return) Nothing

-- | XXX
mapError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
mapError f action = runExceptT action >>= either (throwError . f) return
