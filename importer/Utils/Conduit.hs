{-# LANGUAGE LambdaCase #-}

module Utils.Conduit(awaitWith)
 where

import Data.Conduit(Conduit, await)

awaitWith :: Monad m => (i -> Conduit i m o) -> Conduit i m o
awaitWith fn = await >>= \case
    Nothing -> return ()
    Just v  -> fn v
