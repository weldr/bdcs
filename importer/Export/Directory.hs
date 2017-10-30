-- Copyright (C) 2017 Red Hat, Inc.
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Export.Directory(directorySink)
 where

import           Control.Conditional(unlessM)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.Conduit(Consumer, awaitForever)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           System.Directory(createDirectoryIfMissing, setModificationTime)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createNamedPipe, createSymbolicLink, directoryMode, fileTypeModes, intersectFileModes, namedPipeMode, setFileMode, symbolicLinkMode)
import           System.Posix.Types(CMode(..))

import qualified BDCS.CS as CS
import           BDCS.DB
import           Utils.Filesystem(doesPathExist)

directorySink :: (MonadError String m, MonadIO m) => FilePath -> Consumer (Files, CS.Object) m ()
directorySink outPath = awaitForever $ \obj -> case obj of
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
