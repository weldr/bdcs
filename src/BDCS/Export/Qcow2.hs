{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.Export.Qcow2
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions for exporting objects from the BDCS into a qcow2 image.

module BDCS.Export.Qcow2(qcow2Sink)
 where

import Control.Monad.Except(MonadError)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(MonadLoggerIO, logDebugN)
import Control.Monad.Trans.Resource(MonadResource)
import Data.Conduit(Consumer, bracketP)
import System.Directory(removePathForcibly)
import System.FilePath(takeDirectory)
import System.IO.Temp(createTempDirectory)
import System.Process(callProcess)

import qualified BDCS.CS as CS
import           BDCS.DB(Files)
import           BDCS.Export.Directory(directorySink)
import           BDCS.Export.Utils(runHacks, runTmpfiles)

-- | A 'Consumer' that writes objects into a temporary directory, and then converts that directory into
-- a qcow2 image with virt-make-fs.
qcow2Sink :: (MonadError String m, MonadLoggerIO m, MonadResource m) => FilePath -> Consumer (Files, CS.Object) m ()
qcow2Sink outPath =
    -- Writing and importing a tar file probably will not work, because some rpms contain paths
    -- with symlinks (e.g., /lib64/libaudit.so.1 is expected to be written to /usr/lib64).
    -- Instead, export to a temp directory and convert that to qcow

    bracketP (createTempDirectory (takeDirectory outPath) "export")
        removePathForcibly
        (\tmpDir -> do
            -- Apply tmpfiles.d to the directory first
            logDebugN "Running tmpfiles"
            runTmpfiles tmpDir

            -- Run the sink to create a directory export
            logDebugN "Exporting to directory"
            directorySink tmpDir

            -- Make the direcotry export something usable, hopefully
            logDebugN "Running standard hacks"
            runHacks tmpDir

            -- Run virt-make-fs to generate the qcow2
            logDebugN "Calling virt-make-fs to generate qcow2 image"
            liftIO $ callProcess "virt-make-fs" [tmpDir, outPath, "--format=qcow2", "--label=composer"]
        )
