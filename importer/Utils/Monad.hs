module Utils.Monad(concatForM,
                   concatMapM)
 where

concatForM :: (Monad m, Traversable t) => t a -> (a -> m [b]) -> m [b]
concatForM lst fn = fmap concat (mapM fn lst)

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = fmap concat (mapM fn lst)
