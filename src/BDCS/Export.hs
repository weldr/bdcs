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

module BDCS.Export(export,
                   exportAndCustomize)
 where

import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.Logger(MonadLoggerIO, logDebugN)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource)
import           Data.Conduit(Consumer, (.|), runConduit, runConduitRes)
import qualified Data.Conduit.List as CL
import           Data.ContentStore(openContentStore)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Database.Esqueleto(SqlPersistT)

import qualified BDCS.CS as CS
import           BDCS.DB(Files)
import qualified BDCS.Export.Directory as Directory
import           BDCS.Export.Customize(Customization, filesToObjectsC, runCustomizations)
import           BDCS.Export.FSTree(filesToTree, fstreeSource)
import qualified BDCS.Export.Ostree as Ostree
import qualified BDCS.Export.Qcow2 as Qcow2
import qualified BDCS.Export.Tar as Tar
import           BDCS.Export.Types(ExportType(..))
import           BDCS.Export.Utils(runHacks, runTmpfiles)
import           BDCS.Files(groupIdToFilesC)
import           BDCS.Groups(getGroupIdC)

export :: (MonadBaseControl IO m, MonadError String m, MonadLoggerIO m, MonadResource m) =>
          FilePath
       -> FilePath
       -> ExportType
       -> [T.Text]
       -> SqlPersistT m ()
export repo out_path ty things = exportAndCustomize repo out_path ty things []

exportAndCustomize :: (MonadBaseControl IO m, MonadError String m, MonadLoggerIO m, MonadResource m) =>
                      FilePath
                   -> FilePath
                   -> ExportType
                   -> [T.Text]
                   -> [Customization]
                   -> SqlPersistT m ()
exportAndCustomize repo out_path ty things custom | kernelMissing ty things = throwError "ERROR: ostree exports need a kernel package included"
                                                  | otherwise               = do
    let objectSink = case ty of
                         ExportTar       -> CS.objectToTarEntry .| Tar.tarSink out_path
                         ExportQcow2     -> Qcow2.qcow2Sink out_path
                         ExportOstree    -> Ostree.ostreeSink out_path
                         ExportDirectory -> directoryOutput out_path

    runExceptT (openContentStore repo) >>= \case
        Left e   -> throwError $ show e
        Right cs -> do
            fstree <- runConduit $ CL.sourceList things
                          .| getGroupIdC
                          .| groupIdToFilesC
                          .| filesToTree

            let overlay = Map.empty
            (overlay', fstree') <- runCustomizations overlay cs fstree custom

            runConduitRes $ fstreeSource fstree' .| filesToObjectsC overlay' cs .| objectSink
 where
    directoryOutput :: (MonadError String m, MonadLoggerIO m) => FilePath -> Consumer (Files, CS.Object) m ()
    directoryOutput path = do
        -- Apply tmpfiles.d to the directory first
        logDebugN "Running tmpfiles"
        runTmpfiles path

        Directory.directorySink path
        logDebugN "Running standard hacks"
        runHacks path

    kernelMissing :: ExportType -> [T.Text] -> Bool
    kernelMissing exportTy lst = exportTy == ExportOstree && not (any ("kernel-" `T.isPrefixOf`) lst)
