{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: BDCS.Export
-- Copyright: (c) 2017-2018 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Top-level function for exporting objects from the BDCS.

module BDCS.Export(export)
 where

import           Control.Conditional(cond)
import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource)
import           Data.Conduit(Consumer, (.|), runConduit, runConduitRes)
import           Data.Conduit.List as CL
import           Data.ContentStore(openContentStore)
import           Data.List(isSuffixOf)
import qualified Data.Text as T
import           Database.Esqueleto(SqlPersistT)

import qualified BDCS.CS as CS
import           BDCS.DB(Files)
import qualified BDCS.Export.Directory as Directory
import           BDCS.Export.FSTree(filesToTree, fstreeSource)
import qualified BDCS.Export.Ostree as Ostree
import qualified BDCS.Export.Qcow2 as Qcow2
import qualified BDCS.Export.Tar as Tar
import           BDCS.Export.Utils(runHacks, runTmpfiles)
import           BDCS.Files(groupIdToFilesC)
import           BDCS.Groups(getGroupIdC)

export :: (MonadBaseControl IO m, MonadError String m, MonadIO m, MonadResource m) => FilePath -> FilePath -> [T.Text] -> SqlPersistT m ()
export repo out_path things | kernelMissing out_path things = throwError "ERROR: ostree exports need a kernel package included"
                            | otherwise                     = do
    let objectSink = cond [(".tar" `isSuffixOf` out_path,   CS.objectToTarEntry .| Tar.tarSink out_path),
                           (".qcow2" `isSuffixOf` out_path, Qcow2.qcow2Sink out_path),
                           (".repo" `isSuffixOf` out_path,  Ostree.ostreeSink out_path),
                           (otherwise,                      directoryOutput out_path)]

    runExceptT (openContentStore repo) >>= \case
        Left e   -> throwError $ show e
        Right cs -> do
            fstree <- runConduit $ CL.sourceList things
                          .| getGroupIdC
                          .| groupIdToFilesC
                          .| filesToTree
            runConduitRes $ fstreeSource fstree .| CS.filesToObjectsC cs .| objectSink
 where
    directoryOutput :: (MonadError String m, MonadIO m) => FilePath -> Consumer (Files, CS.Object) m ()
    directoryOutput path = do
        -- Apply tmpfiles.d to the directory first
        liftIO $ runTmpfiles path

        Directory.directorySink path
        liftIO $ runHacks path

    kernelMissing :: FilePath -> [T.Text] -> Bool
    kernelMissing out lst = ".repo" `isSuffixOf` out && not (any ("kernel-" `T.isPrefixOf`) lst)
