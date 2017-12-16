-- |
-- Module: BDCS.Utils.Either
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Utility functions to help with Either values

module BDCS.Utils.Either(maybeToEither,
                         whenLeft,
                         whenRight)
 where

import Control.Monad.Except(MonadError, throwError)

-- | Throw the passed err if the value is Nothing, otherwise return the value.
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither err Nothing = throwError err
maybeToEither _ (Just v)  = return v

-- | Run a function on the Left error, otherwise do nothing
whenLeft :: Monad m => Either e a -> (e -> m ()) -> m ()
whenLeft (Left a) fn = fn a
whenLeft _        _  = return ()

-- | Run a function on the Right value, otherwise do nothing
whenRight :: Monad m => Either e a -> (a -> m ()) -> m ()
whenRight (Right a) fn = fn a
whenRight _         _  = return ()
