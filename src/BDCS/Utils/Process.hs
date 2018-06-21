{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Control.Exception.Lifted as CEL
import           Control.Monad(unless)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Logger(MonadLoggerIO, logInfoN, logErrorN)
import           Control.Monad.Trans.Control(MonadBaseControl)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           GHC.IO.Exception(IOErrorType(..))
import           System.IO.Error(mkIOError)
import           System.Process.Typed(ExitCodeException(..), proc, readProcess_)

callProcessLogged :: (MonadBaseControl IO m, MonadLoggerIO m) => String -> [String] -> m ()
callProcessLogged cmd args =
    doit `CEL.catches` [CEL.Handler (\(e :: ExitCodeException) -> handleProcessFailed e),
                        CEL.Handler (\(e :: CEL.SomeException) -> handleOtherErrors e)]
 where
    handleProcessFailed :: (MonadBaseControl IO m, MonadLoggerIO m) => ExitCodeException -> m ()
    handleProcessFailed ExitCodeException{..} = do
        logErrorN $ cs eceStderr
        liftIO $ ioError (mkIOError OtherError (cmd ++ unwords args ++ " (" ++ show eceExitCode ++ ")")
                                    Nothing Nothing)

    handleOtherErrors :: (MonadBaseControl IO m, MonadLoggerIO m, CEL.Exception e) => e -> m ()
    handleOtherErrors e = do
        logErrorN $ cs $ show e
        liftIO $ ioError (mkIOError OtherError (cmd ++ unwords args ++ " (" ++ show e ++ ")")
                                    Nothing Nothing)

    doit :: (MonadBaseControl IO m, MonadLoggerIO m) => m ()
    doit = do
        logInfoN $ T.intercalate " " $ T.pack cmd : map T.pack args
        (stdout, _) <- liftIO $ readProcess_ (proc cmd args)

        unless (T.null $ T.strip $ cs stdout) $
            logInfoN $ cs stdout
