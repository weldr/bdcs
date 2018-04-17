{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Export.Directory
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions for exporting objects from the BDCS into a directory.

module BDCS.Export.Directory(directorySink)
 where

import           Control.Conditional(unlessM)
import           Control.Exception(IOException)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Logger(MonadLoggerIO)
import           Control.Monad.Trans.Control(MonadBaseControl)
import qualified Data.ByteString as BS
import           Data.Conduit(Consumer, awaitForever, handleC)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           System.Directory(createDirectoryIfMissing, setModificationTime)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createNamedPipe, createSymbolicLink, directoryMode, fileTypeModes, intersectFileModes, namedPipeMode, setFileMode, symbolicLinkMode)
import           System.Posix.Types(CMode(..))

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Utils.Filesystem(doesPathExist)

-- | A 'Consumer' that writes objects into a provided directory.  Symlinks and other file-like
-- objects will be handled properly.  Only some metadata is currently handled.  Various errors
-- can be thrown depending on problems encountered when interacting with the filesystem.
--
-- It is expected that the caller will decide whether the destination directory should be empty
-- or not.  This function does nothing to enforce that.
directorySink :: (MonadBaseControl IO m, MonadError String m, MonadLoggerIO m) => FilePath -> Consumer (Files, CS.Object) m ()
directorySink outPath =  handleC (\e -> throwError $ show (e :: IOException)) $ awaitForever $ \case
    (f, CS.SpecialObject) -> checkoutSpecial f
    (f, CS.FileObject bs) -> checkoutFile f bs
 where
    checkoutSpecial :: (MonadError String m, MonadIO m) => Files -> m ()
    checkoutSpecial f@Files{..} =
        let fullPath = outPath </> dropDrive (T.unpack filesPath)
            fileType = fromIntegral filesMode `intersectFileModes` fileTypeModes
        in
           if | fileType == symbolicLinkMode -> checkoutSymlink fullPath f
              | fileType == directoryMode    -> liftIO $ createDirectoryIfMissing True fullPath >> setMetadata f fullPath
              | fileType == namedPipeMode    -> liftIO $ createNamedPipe fullPath $ fromIntegral filesMode
              -- TODO, not storing major/minor for char/block special
              | otherwise -> throwError "Invalid file type"

    checkoutSymlink :: (MonadError String m, MonadIO m) => FilePath -> Files -> m ()
    checkoutSymlink _ Files{filesTarget=Nothing, ..} = throwError "Missing symlink target"
    checkoutSymlink fullPath Files{filesTarget=Just target, ..} =
        -- Skip creating the symbolic link if the target already exists
        liftIO $ unlessM (doesPathExist fullPath) (createSymbolicLink (T.unpack target) fullPath)

    checkoutFile :: MonadIO m => Files -> BS.ByteString -> m ()
    checkoutFile f@Files{..} contents = liftIO $ do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        createDirectoryIfMissing True $ takeDirectory fullPath

        BS.writeFile fullPath contents
        setMetadata f fullPath

    setMetadata :: Files -> FilePath -> IO ()
    setMetadata Files{..} fullPath = do
        -- set the mode
        setFileMode fullPath (CMode $ fromIntegral filesMode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs
