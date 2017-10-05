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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Export.Directory(directorySink)
 where

import           Control.Conditional(unlessM)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.Conduit(Consumer, awaitForever)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           System.Directory(createDirectoryIfMissing, doesPathExist, setModificationTime)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))

import qualified BDCS.CS as CS
import           BDCS.DB

directorySink :: MonadIO m => FilePath -> Consumer (Files, CS.Object) m ()
directorySink outPath = awaitForever $ \obj -> case obj of
    (f, CS.DirObject)     -> liftIO (checkoutDir f)
    (f, CS.FileObject bs) -> liftIO (checkoutFile f bs)
 where
    checkoutDir :: Files -> IO ()
    checkoutDir f@Files{..} = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        -- create the directory if it isn't there already
        createDirectoryIfMissing True fullPath

        setMetadata f fullPath

    checkoutFile :: Files -> BS.ByteString -> IO ()
    checkoutFile f@Files{..} contents = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        createDirectoryIfMissing True $ takeDirectory fullPath

        -- Write the data or the symlink, depending
        -- Skip creating the symbolic link if the target already exists
        case (filesTarget, contents) of
            (Just target, _) -> unlessM (doesPathExist fullPath) (createSymbolicLink (T.unpack target) fullPath)
            (_, c)           -> do
                BS.writeFile fullPath c
                setMetadata f fullPath

    setMetadata :: Files -> FilePath -> IO ()
    setMetadata Files{..} fullPath = do
        -- set the mode
        setFileMode fullPath (CMode $ fromIntegral filesMode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs
