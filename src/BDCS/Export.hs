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
import           Control.Monad.Except(MonadError, runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.Conduit(Consumer, (.|), runConduit, runConduitRes)
import           Data.Conduit.List as CL
import           Data.ContentStore(openContentStore, runCsMonad)
import           Data.List(isSuffixOf)
import qualified Data.Text as T
import           System.Directory(removePathForcibly)

import qualified BDCS.CS as CS
import           BDCS.DB(Files, checkAndRunSqlite)
import qualified BDCS.Export.Directory as Directory
import           BDCS.Export.FSTree(filesToTree, fstreeSource)
import qualified BDCS.Export.Ostree as Ostree
import qualified BDCS.Export.Qcow2 as Qcow2
import qualified BDCS.Export.Tar as Tar
import           BDCS.Export.Utils(runHacks, runTmpfiles)
import           BDCS.Files(groupIdToFilesC)
import           BDCS.Groups(getGroupIdC)

export :: FilePath -> FilePath -> FilePath -> [T.Text] -> IO (Either String ())
export db repo out_path things | kernelMissing out_path things = return $ Left "ERROR: ostree exports need a kernel package included"
                               | otherwise                     = runCsMonad (openContentStore repo) >>= \case
    Left e   -> return $ Left $ show e
    Right cs -> do
        let (handler, objectSink) = cond [(".tar" `isSuffixOf` out_path,   (removePathForcibly out_path, CS.objectToTarEntry .| Tar.tarSink out_path)),
                                          (".qcow2" `isSuffixOf` out_path, (removePathForcibly out_path, Qcow2.qcow2Sink out_path)),
                                          (".repo" `isSuffixOf` out_path,  (removePathForcibly out_path, Ostree.ostreeSink out_path)),
                                          (otherwise,                      (return (), directoryOutput out_path))]

        result <- runExceptT $ do
            -- Build the filesystem tree to export
            fstree <- checkAndRunSqlite (T.pack db) $ runConduit $ CL.sourceList things
                        .| getGroupIdC
                        .| groupIdToFilesC
                        .| filesToTree

            -- Traverse the tree and export the file contents
            runConduitRes $ fstreeSource fstree .| CS.filesToObjectsC cs .| objectSink

        case result of
            Left e  -> handler >> return (Left e)
            Right _ -> return $ Right ()
 where
    directoryOutput :: (MonadError String m, MonadIO m) => FilePath -> Consumer (Files, CS.Object) m ()
    directoryOutput path = do
        -- Apply tmpfiles.d to the directory first
        liftIO $ runTmpfiles path

        Directory.directorySink path
        liftIO $ runHacks path

    kernelMissing :: FilePath -> [T.Text] -> Bool
    kernelMissing out lst = ".repo" `isSuffixOf` out && not (any ("kernel-" `T.isPrefixOf`) lst)
