{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.Utils.Process
-- Copyright: (c) 2018 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions for running external commands with logging.

module BDCS.Utils.Process(callProcessLogged)
 where

import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(MonadLoggerIO, logInfoN, logErrorN)
import qualified Data.Text as T
import           GHC.IO.Exception(IOErrorType(..))
import           System.Exit(ExitCode(..))
import           System.IO.Error(mkIOError)
import           System.Process(readProcessWithExitCode)

callProcessLogged :: MonadLoggerIO m => String -> [String] -> m ()
callProcessLogged cmd args = do
    logInfoN $ T.intercalate " " $ T.pack cmd : map T.pack args
    (rc, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args ""
    logInfoN $ T.pack stdout

    case rc of
        ExitFailure x -> do logErrorN $ T.pack stderr
                            liftIO $ ioError (mkIOError OtherError (cmd ++ unwords args ++ " (" ++ show x ++ ")")
                                                        Nothing Nothing)
        _             -> return ()
