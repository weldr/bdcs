module Utils.Monad(concatMapM)
 where

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = fmap concat (mapM fn lst)
