module Utils.Either(maybeToEither)
 where

import Control.Monad.Except(MonadError, throwError)

maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither err Nothing = throwError err
maybeToEither _ (Just v)  = return v
