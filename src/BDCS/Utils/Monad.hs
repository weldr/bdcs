{-# LANGUAGE LambdaCase #-}

-- |
-- Module: BDCS.Utils.Monad
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Monad related utility functions

module BDCS.Utils.Monad(concatForM,
                        concatMapM,
                        foldMaybeM,
                        mapMaybeM,
                        (>>?))
 where

import Data.Maybe(catMaybes)

-- | XXX
concatForM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
concatForM lst fn = fmap concat (mapM fn lst)

-- | XXX
concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = fmap concat (mapM fn lst)

-- | XXX
mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM fn = fmap catMaybes . mapM fn

-- | XXX
-- foldM, but skip Nothing results
foldMaybeM :: (Monad m) => (b -> a -> m (Maybe b)) -> b -> [a] -> m b
foldMaybeM _ acc [] = return acc
foldMaybeM action acc (x:xs) = do
    result <- action acc x
    case result of
        -- skip this element, continue with the original accumulator
        Nothing -> foldMaybeM action acc xs
        -- Keep this one
        Just r  -> foldMaybeM action r xs

-- | XXX
-- compose a monadic action and Maybe
infixl 1 >>?
(>>?) :: Monad m => m (Maybe a) -> (a -> m b) -> m (Maybe b)
(>>?) input action = input >>= \case
    Nothing -> return Nothing
    Just x  -> Just <$> action x
