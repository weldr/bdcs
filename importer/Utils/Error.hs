module Utils.Error(errorToEither,
                   errorToMaybe)

 where

import Control.Monad.Except(MonadError, catchError)

-- Convert an error action into an Either
-- This is essentially runExceptT generalized to MonadError
errorToEither :: MonadError e m => m a -> m (Either e a)
errorToEither action = (Right <$> action) `catchError` (return . Left)

-- Same as above, but discard the error
errorToMaybe :: MonadError e m => m a -> m (Maybe a)
errorToMaybe action = (Just <$> action) `catchError` (const . return) Nothing
