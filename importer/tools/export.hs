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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Codec.Archive.Tar as Tar
import           Control.Conditional(ifM, whenM)
import           Control.Monad(when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(runResourceT)
import           Data.ByteString.Lazy(writeFile)
import           Data.Conduit((.|), Consumer, await, runConduit)
import           Data.Conduit.Binary(sinkFile)
import qualified Data.Conduit.List as CL
import           Data.List(isSuffixOf, isPrefixOf, partition)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Database.Persist.Sqlite(runSqlite)
import           Prelude hiding(writeFile)
import           System.Directory(createDirectoryIfMissing, doesFileExist, removeFile, setModificationTime)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(groupIdToFilesC)
import           BDCS.Groups(getGroupIdC)
import           BDCS.Version
import           Utils.Conduit(sourceInputStream)
import           Utils.Either(whenLeft)
import           Utils.Monad(concatMapM)

tarSink :: MonadIO m => FilePath -> Consumer Tar.Entry m ()
tarSink out_path = do
    entries <- CL.consume
    liftIO $ writeFile out_path (Tar.write entries)

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

-- | Check a list of strings to see if any of them are files
-- If it is, read it and insert its contents in its place
expandFileThings :: [String] -> IO [String]
expandFileThings = concatMapM isThingFile
  where
    isThingFile :: String ->  IO [String]
    isThingFile thing = ifM (doesFileExist thing)
                            (lines <$> readFile thing)
                            (return [thing])

usage :: IO ()
usage = do
    printVersion "export"
    putStrLn "Usage: export metadata.db repo dest thing [thing ...]"
    putStrLn "dest can be:"
    putStrLn "\t* A directory (which may or may not already exist)"
    putStrLn "\t* The name of a .tar file to be created"
    putStrLn "thing can be:"
    putStrLn "\t* The name of an RPM"
    putStrLn "\t* A path to a file containing names of RPMs, 1 per line."
    -- TODO group id?
    exitFailure

needFilesystem :: IO ()
needFilesystem = do
    printVersion "export"
    putStrLn "ERROR: The tar needs to have the filesystem package included"
    exitFailure

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 4) usage

    let db_path = T.pack (argv !! 0)
    repo <- CS.open (argv !! 1)
    let out_path = argv !! 2
    allThings <- expandFileThings $ drop 3 argv

    let (match, otherThings) = partition (isPrefixOf "filesystem-") allThings
    when (length match < 1) needFilesystem
    let things = map T.pack $ match !! 0 : otherThings

    let (handler, objectSink) = if ".tar" `isSuffixOf` out_path
            then (\e -> print e >> whenM (doesFileExist out_path) (removeFile out_path), CS.objectToTarEntry .| tarSink out_path)
            else (print, directorySink out_path)

    result <- runExceptT $ runSqlite db_path $ runConduit $ CL.sourceList things
        .| getGroupIdC
        .| groupIdToFilesC
        .| CS.filesToObjectsC repo
        .| objectSink

    whenLeft result handler
