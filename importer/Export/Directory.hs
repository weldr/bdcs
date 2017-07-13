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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Export.Directory(directorySink)
 where

import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(runResourceT)
import           Data.Conduit((.|), Consumer, await, runConduit)
import           Data.Conduit.Binary(sinkFile)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           System.Directory(createDirectoryIfMissing, setModificationTime)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Version
import           Utils.Conduit(sourceInputStream)

directorySink :: MonadIO m => FilePath -> Consumer (Files, CS.Object) m ()
directorySink outPath = await >>= \case
    Nothing                         -> return ()
    Just (f, CS.DirMeta dirmeta)    -> liftIO (checkoutDir f dirmeta)  >> directorySink outPath
    Just (f, CS.FileObject fileObj) -> liftIO (checkoutFile f fileObj) >> directorySink outPath
 where
    checkoutDir :: Files -> CS.Metadata -> IO ()
    checkoutDir f@Files{..} metadata = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        -- create the directory if it isn't there already
        createDirectoryIfMissing True fullPath

        setMetadata f fullPath metadata

    checkoutFile :: Files -> CS.FileContents -> IO ()
    checkoutFile f@Files{..} CS.FileContents{..} = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        createDirectoryIfMissing True $ takeDirectory fullPath

        -- Write the data or the symlink, depending
        case (symlink, contents) of
            (Just symlinkTarget, _) -> createSymbolicLink (T.unpack symlinkTarget) fullPath
            (_, Just c)             -> do
                runResourceT $ runConduit $ sourceInputStream c .| sinkFile fullPath
                setMetadata f fullPath metadata
            -- TODO?
            _                       -> return ()

    setMetadata :: Files -> FilePath -> CS.Metadata -> IO ()
    setMetadata Files{..} fullPath CS.Metadata{..} = do
        -- set the mode
        setFileMode fullPath (CMode mode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs
