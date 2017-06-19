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
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Conditional(ifM, whenM)
import           Control.Monad(unless, when)
import           Control.Monad.Except(ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, runResourceT)
import           Data.ByteString(ByteString)
import           Data.ByteString.Lazy(writeFile)
import           Data.Conduit((.|), Conduit, Consumer, Producer, await, runConduit, yield)
import           Data.Conduit.Binary(sinkFile, sinkLbs)
import qualified Data.Conduit.List as CL
import           Data.List(isSuffixOf, isPrefixOf, partition)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           Prelude hiding(writeFile)
import           System.Directory(createDirectoryIfMissing, doesFileExist, removeFile, setModificationTime)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))

import           GI.Gio(IsInputStream, inputStreamReadBytes, noCancellable)
import           GI.GLib(bytesGetData, bytesGetSize)
import           GI.OSTree(IsRepo)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Groups(nevraToGroupId)
import           BDCS.RPM.Utils(splitFilename)
import           BDCS.Version
import           Utils.Either(maybeToEither, whenLeft)
import           Utils.Monad(concatMapM)

-- Convert a GInputStream to a conduit source
sourceInputStream :: (MonadResource m, IsInputStream i) => i -> Producer m ByteString
sourceInputStream input = do
    let buf_size = 8096
    bytes <- liftIO $ inputStreamReadBytes input buf_size noCancellable
    bytesSize <- liftIO $ bytesGetSize bytes
    unless (bytesSize == 0) $ do
        bytesData <- liftIO $ bytesGetData bytes
        yield $ fromMaybe "" bytesData
        sourceInputStream input

groupIdToFiles :: (MonadBaseControl IO m, MonadIO m) => T.Text -> Conduit (Key Groups) m Files
groupIdToFiles db_path = await >>= \case
    Nothing      -> return ()
    -- TODO: use selectSource
    Just groupid -> do
        files <- lift $ runSqlite db_path $ runQuery groupid
        CL.sourceList files
        groupIdToFiles db_path
 where
    runQuery :: (MonadBaseControl IO m, MonadIO m) => Key Groups -> SqlPersistT m [Files]
    runQuery groupid = do
        files <- select $ from $ \(files `InnerJoin` group_files) -> do
                 on     $ files ^. FilesId ==. group_files ^. GroupFilesFile_id
                 where_ $ group_files ^. GroupFilesGroup_id ==. val groupid
                 return files
        return $ map entityVal files

getGroupIdC :: (MonadError String m, MonadBaseControl IO m, MonadIO m) => T.Text -> Conduit T.Text m (Key Groups)
getGroupIdC db_path = await >>= \case
    Nothing    -> return ()
    Just thing -> do
        maybeId <- lift $ runSqlite db_path $ nevraToGroupId (splitFilename thing)
        case maybeId of
            Just gid -> yield gid >> getGroupIdC db_path
            Nothing  -> throwError $ "No such group " ++ T.unpack thing

filesToObjectsC :: (IsRepo a, MonadError String m, MonadIO m) => a -> Conduit Files m (Files, CS.Object)
filesToObjectsC repo = await >>= \case
    Nothing        -> return ()
    Just f@Files{..} -> case filesCs_object of
        Nothing       -> filesToObjectsC repo
        Just checksum -> do
            object <- CS.load repo checksum
            yield (f, object)
            filesToObjectsC repo

objectToTarEntry :: (MonadError String m, MonadIO m) => Conduit (Files, CS.Object) m Tar.Entry
objectToTarEntry = await >>= \case
    Nothing                 -> return ()
    Just (f@Files{..}, obj) -> do
        result <- case obj of
                CS.DirMeta dirmeta    -> return $ checkoutDir f dirmeta
                CS.FileObject fileObj -> liftIO . runExceptT $ checkoutFile f fileObj

        either (\e -> throwError $ "Could not checkout out object " ++ T.unpack filesPath ++ ": " ++ e)
               yield
               result

        objectToTarEntry
 where
    checkoutDir :: Files -> CS.Metadata -> Either String Tar.Entry
    checkoutDir f@Files{..} metadata@CS.Metadata{..} = do
        path <- Tar.toTarPath True (T.unpack filesPath)
        return $ setMetadata f metadata (Tar.directoryEntry path)

    checkoutSymlink :: Files -> CS.Metadata -> T.Text -> Either String Tar.Entry
    checkoutSymlink f@Files{..} metadata target = do
        path'   <- Tar.toTarPath False (T.unpack filesPath)
        target' <- maybeToEither ("Path is too long or contains invalid characters: " ++ T.unpack target)
                                 (Tar.toLinkTarget (T.unpack target))
        return $ setMetadata f metadata (Tar.simpleEntry path' (Tar.SymbolicLink target'))

    checkoutFile :: Files -> CS.FileContents -> ExceptT String IO Tar.Entry
    checkoutFile f CS.FileContents{symlink=Just target, ..} =
        ExceptT $ return $ checkoutSymlink f metadata target
    checkoutFile f@Files{..} CS.FileContents{symlink=Nothing, ..} = do
        path         <- ExceptT $ return $ Tar.toTarPath False (T.unpack filesPath)
        lazyContents <- runResourceT $ runConduit $ sourceInputStream contents .| sinkLbs

        return $ setMetadata f metadata (Tar.fileEntry path lazyContents)

    setMetadata :: Files -> CS.Metadata -> Tar.Entry -> Tar.Entry
    setMetadata Files{..} metadata entry =
        entry { Tar.entryPermissions = CMode (CS.mode metadata),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = fromIntegral (CS.uid metadata),
                                                       Tar.groupId = fromIntegral (CS.gid metadata),
                                                       Tar.ownerName = "",
                                                       Tar.groupName = "" },
                Tar.entryTime = fromIntegral filesMtime }

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
        case symlink of
            Just symlinkTarget -> createSymbolicLink (T.unpack symlinkTarget) fullPath
            Nothing -> do
                runResourceT $ runConduit $ sourceInputStream contents .| sinkFile fullPath
                setMetadata f fullPath metadata

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
            then (\e -> print e >> whenM (doesFileExist out_path) (removeFile out_path), objectToTarEntry .| tarSink out_path)
            else (print, directorySink out_path)

    result <- runExceptT $ runConduit $ CL.sourceList things
        .| getGroupIdC db_path
        .| groupIdToFiles db_path
        .| filesToObjectsC repo
        .| objectSink

    whenLeft result handler
