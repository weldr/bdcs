module Utils.Either(maybeToEither,
                    whenLeft,
                    whenRight)
 where

import Control.Monad.Except(MonadError, throwError)

maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither err Nothing = throwError err
maybeToEither _ (Just v)  = return v

whenLeft :: Monad m => Either e a -> (e -> m ()) -> m ()
whenLeft (Left a) fn = fn a
whenLeft _        _  = return ()

whenRight :: Monad m => Either e a -> (a -> m ()) -> m ()
whenRight (Right a) fn = fn a
whenRight _         _  = return ()
