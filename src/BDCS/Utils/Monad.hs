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

-- | Like 'Data.List.concatMap', but with its arguments reversed (the list comes
-- first and the function comes second) and operates in a monad.
concatForM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
concatForM lst fn = fmap concat (mapM fn lst)

-- | Like 'Data.List.concatMap' but operates in a monad.
concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = fmap concat (mapM fn lst)

-- | Like 'Data.Maybe.mapMaybe' but operates in a monad.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM fn = fmap catMaybes . mapM fn

-- | XXX
-- foldM, but skip Nothing results
foldMaybeM :: Monad m => (b -> a -> m (Maybe b)) -> b -> [a] -> m b
foldMaybeM _ acc [] = return acc
foldMaybeM action acc (x:xs) = do
    result <- action acc x
    case result of
        -- skip this element, continue with the original accumulator
        Nothing -> foldMaybeM action acc xs
        -- Keep this one
        Just r  -> foldMaybeM action r xs

-- | Apply a function to a monadic action.  If the action is 'm Nothing', do nothing.  Otherwise, apply the
-- function and return the result as 'm (Just a)'.
infixl 1 >>?
(>>?) :: Monad m => m (Maybe a) -> (a -> m b) -> m (Maybe b)
(>>?) input action = input >>= \case
    Nothing -> return Nothing
    Just x  -> Just <$> action x
