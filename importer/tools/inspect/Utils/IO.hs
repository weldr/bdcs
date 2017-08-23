module Utils.IO(liftedPutStrLn)
 where

import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

liftedPutStrLn :: MonadIO m => T.Text -> m ()
liftedPutStrLn = liftIO . TIO.putStrLn
