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

module Export.Qcow2(qcow2Sink)
 where

import Control.Conditional(whenM)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Trans.Resource(MonadResource)
import Data.Conduit(Consumer, bracketP)
import Data.List(intercalate)
import Data.List.Split(splitOn)
import System.Directory(createDirectoryIfMissing, doesFileExist, removePathForcibly, renameFile)
import System.FilePath((</>), takeDirectory)
import System.IO.Temp(createTempDirectory)
import System.Process(callProcess)

import qualified BDCS.CS as CS
import           BDCS.DB(Files)
import           Export.Directory(directorySink)
import           Export.Utils(runHacks)

import Paths_db(getDataFileName)

qcow2Sink :: (MonadResource m, MonadIO m) => FilePath -> Consumer (Files, CS.Object) m ()
qcow2Sink outPath =
    -- Writing and importing a tar file probably will not work, because some rpms contain paths
    -- with symlinks (e.g., /lib64/libaudit.so.1 is expected to be written to /usr/lib64).
    -- Instead, export to a temp directory and convert that to qcow

    bracketP (createTempDirectory (takeDirectory outPath) "export")
        removePathForcibly
        (\tmpDir -> do
            -- Run the sink to create a directory export
            directorySink tmpDir

            -- Make the direcotry export something usable, hopefully
            liftIO $ runHacks tmpDir

            -- Run virt-make-fs to generate the qcow2
            liftIO $ callProcess "virt-make-fs" [tmpDir, outPath, "--format=qcow2", "--label=composer"]
        )
