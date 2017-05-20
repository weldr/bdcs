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
import           Control.Conditional(ifM)
import           Control.Monad(unless, when)
import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Maybe(MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, runResourceT)
import           Data.ByteString(ByteString)
import           Data.ByteString.Lazy(writeFile)
import           Data.Conduit((.|), Producer, runConduit, yield)
import           Data.Conduit.Binary(sinkFile, sinkLbs)
import qualified Data.Conduit.List as CL
import           Data.List(isSuffixOf)
import           Data.Maybe(fromMaybe)
import           Data.Text(Text, pack, unpack)
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           Prelude hiding(writeFile)
import           System.Directory(createDirectoryIfMissing, doesFileExist, setModificationTime)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))

import GI.Gio    hiding(on)
import GI.GLib   hiding(String, on)
import GI.OSTree hiding(on)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(groupIdToFiles)
import           BDCS.Groups(nameToGroupId)
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

checkoutObjectToDisk :: (MonadError String m, MonadIO m, IsRepo a) => a -> FilePath -> Files -> m ()
checkoutObjectToDisk repo outPath Files{..} =
    case filesCs_object of
        Just checksum -> CS.load repo checksum >>= liftIO . \case
                            CS.DirMeta dirmeta -> checkoutDir dirmeta
                            CS.FileObject fileObj -> checkoutFile fileObj
        Nothing       -> return ()

  where
    checkoutDir :: CS.Metadata -> IO ()
    checkoutDir metadata = do
        let fullPath = outPath </> dropDrive (unpack filesPath)

        -- create the directory if it isn't there already
        createDirectoryIfMissing True fullPath

        setMetadata fullPath metadata

    checkoutFile :: CS.FileContents -> IO ()
    checkoutFile CS.FileContents{..} = do
        let fullPath = outPath </> dropDrive (unpack filesPath)

        createDirectoryIfMissing True $ takeDirectory fullPath

        -- Write the data or the symlink, depending
        case symlink of
            Just symlinkTarget -> createSymbolicLink (unpack symlinkTarget) fullPath
            Nothing -> runResourceT $ runConduit $ sourceInputStream contents .| sinkFile fullPath

        setMetadata fullPath metadata

    setMetadata :: FilePath -> CS.Metadata -> IO ()
    setMetadata fullPath CS.Metadata{..} = do
        -- set the mode
        setFileMode fullPath (CMode mode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs

checkoutObjectToTarEntry :: (MonadError String m, MonadIO m, IsRepo a) => a -> Files -> m (Maybe Tar.Entry)
checkoutObjectToTarEntry repo Files{..} =
    case filesCs_object of
        Just checksum -> CS.load repo checksum >>= liftIO . \case
                            CS.DirMeta dirmeta    -> runMaybeT $ checkoutDir dirmeta
                            CS.FileObject fileObj -> runMaybeT $ checkoutFile fileObj
        Nothing       -> return Nothing
 where
    mkTarPath :: Text -> Bool -> Maybe Tar.TarPath
    mkTarPath s isDir = case Tar.toTarPath isDir (unpack s) of
        Left _  -> Nothing
        Right p -> Just p

    checkoutDir :: CS.Metadata -> MaybeT IO Tar.Entry
    checkoutDir metadata@CS.Metadata{..} = do
        path <- MaybeT $ return $ mkTarPath filesPath True
        return $ setMetadata metadata (Tar.directoryEntry path)

    checkoutFile :: CS.FileContents -> MaybeT IO Tar.Entry
    checkoutFile CS.FileContents{symlink=Nothing, ..} = do
        path         <- MaybeT $ return $ mkTarPath filesPath False
        lazyContents <- runResourceT $ runConduit $ sourceInputStream contents .| sinkLbs

        return $ setMetadata metadata (Tar.fileEntry path lazyContents)

    checkoutFile CS.FileContents{symlink=Just t, ..} = do
        path      <- MaybeT $ return $ mkTarPath filesPath False
        target    <- MaybeT $ return $ Tar.toLinkTarget (unpack t)
        let entry  = Tar.simpleEntry path (Tar.SymbolicLink target)

        return $ setMetadata metadata entry

    setMetadata :: CS.Metadata -> Tar.Entry -> Tar.Entry
    setMetadata metadata entry =
        entry { Tar.entryPermissions = CMode (CS.mode metadata),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = fromIntegral (CS.uid metadata),
                                                       Tar.groupId = fromIntegral (CS.gid metadata),
                                                       Tar.ownerName = "",
                                                       Tar.groupName = "" },
                Tar.entryTime = fromIntegral filesMtime }

getGroupId :: (MonadError String m, MonadIO m) => Text -> SqlPersistT m (Key Groups)
getGroupId thing = nameToGroupId thing >>= \case
    Just gid -> return gid
    Nothing  -> throwError $ "No such group " ++ unpack thing

processOneThingToDir :: (MonadError String m, MonadResource m, IsRepo r) => r -> FilePath -> Text -> SqlPersistT m ()
processOneThingToDir repo outPath thing = do
    -- Get the group id of the thing
    groupId <- getGroupId thing

    -- Get all of the files associated with the group
    runConduit $ groupIdToFiles groupId .| CL.mapM_ (checkoutObjectToDisk repo outPath)

processOneThingToTarEntries :: (MonadError String m, MonadResource m, IsRepo r) => r -> Text -> SqlPersistT m [Tar.Entry]
processOneThingToTarEntries repo thing = do
    -- Get the group id of the thing
    groupId <- getGroupId thing

    -- Get all of the files associated with the group, as entries
    runConduit $ groupIdToFiles groupId .| CL.mapMaybeM (checkoutObjectToTarEntry repo) .| CL.consume

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
    putStrLn "Usage: export metadata.db repo dest thing [thing ...]"
    putStrLn "dest can be:"
    putStrLn "\t* A directory (which may or may not already exist)"
    putStrLn "\t* The name of a .tar file to be created"
    putStrLn "thing can be:"
    putStrLn "\t* The name of an RPM"
    putStrLn "\t* A path to a file containing names of RPMs, 1 per line."
    -- TODO group id?
    exitFailure

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 4) usage

    let db_path = pack (argv !! 0)
    repo <- CS.open (argv !! 1)
    let out_path = argv !! 2
    allThings <- expandFileThings $ drop 3 argv
    let things = map pack allThings

    result <- if ".tar" `isSuffixOf` out_path
              then runExceptT $ runResourceT $ processThingsToTar db_path repo out_path things
              else do createDirectoryIfMissing True out_path
                      runExceptT $ runResourceT $ processThings db_path repo out_path things

    case result of
        Left e  -> print e
        Right _ -> return ()
 where
    processThings :: (MonadError String m, MonadBaseControl IO m, MonadResource m, IsRepo a) => Text -> a -> FilePath -> [Text] -> m ()
    processThings dbPath repo outPath things = runSqlite dbPath $
        mapM_ (processOneThingToDir repo outPath) things

    processThingsToTar :: (MonadError String m, MonadBaseControl IO m, MonadResource m, IsRepo a) => Text -> a -> FilePath -> [Text] -> m ()
    processThingsToTar dbPath repo outPath things = runSqlite dbPath $ do
        entries <- concatMapM (processOneThingToTarEntries repo) things
        liftIO $ writeFile outPath (Tar.write entries)
