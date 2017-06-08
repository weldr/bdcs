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
import           Control.Monad.Except(ExceptT(..), MonadError, catchError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, runResourceT)
import           Data.ByteString(ByteString)
import           Data.ByteString.Lazy(writeFile)
import           Data.Conduit((.|), Producer, runConduit, yield)
import           Data.Conduit.Binary(sinkFile, sinkLbs)
import qualified Data.Conduit.List as CL
import           Data.List(isSuffixOf)
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

import GI.Gio    hiding(on)
import GI.GLib   hiding(String, on)
import GI.OSTree hiding(on)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(groupIdToFiles)
import           BDCS.Groups(nvraToGroupId)
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
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        -- create the directory if it isn't there already
        createDirectoryIfMissing True fullPath

        setMetadata fullPath metadata

    checkoutFile :: CS.FileContents -> IO ()
    checkoutFile CS.FileContents{..} = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        createDirectoryIfMissing True $ takeDirectory fullPath

        -- Write the data or the symlink, depending
        case symlink of
            Just symlinkTarget -> createSymbolicLink (T.unpack symlinkTarget) fullPath
            Nothing -> runResourceT $ runConduit $ sourceInputStream contents .| sinkFile fullPath

        setMetadata fullPath metadata

    setMetadata :: FilePath -> CS.Metadata -> IO ()
    setMetadata fullPath CS.Metadata{..} = do
        -- set the mode
        setFileMode fullPath (CMode mode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs

checkoutObjectToTarEntry :: (MonadError String m, MonadIO m, IsRepo a) => a -> Files -> m Tar.Entry
checkoutObjectToTarEntry repo Files{..} =
    case filesCs_object of
        Just checksum -> do result <- CS.load repo checksum >>= liftIO . \case
                                CS.DirMeta dirmeta    -> return $ checkoutDir dirmeta
                                CS.FileObject fileObj -> runExceptT $ checkoutFile fileObj

                            either (\e -> throwError $ "Could not check out object " ++ T.unpack filesPath ++ ": " ++ e)
                                   return
                                   result
        Nothing       -> throwError $ "Object has no checksum: " ++ T.unpack filesPath
 where
    checkoutDir :: CS.Metadata -> Either String Tar.Entry
    checkoutDir metadata@CS.Metadata{..} = do
        path <- Tar.toTarPath True (T.unpack filesPath)
        return $ setMetadata metadata (Tar.directoryEntry path)

    checkoutSymlink :: CS.FileContents -> Either String Tar.Entry
    checkoutSymlink CS.FileContents{symlink=Nothing, ..} =
        throwError $ T.unpack filesPath ++ " is not a symbolic link"
    checkoutSymlink CS.FileContents{symlink=Just target, ..} = do
        path'   <- Tar.toTarPath False (T.unpack filesPath)
        target' <- maybeToEither ("Path is too long or contains invalid characters: " ++ T.unpack target)
                                 (Tar.toLinkTarget (T.unpack target))
        return $ setMetadata metadata (Tar.simpleEntry path' (Tar.SymbolicLink target'))

    checkoutFile :: CS.FileContents -> ExceptT String IO Tar.Entry
    checkoutFile fc@CS.FileContents{symlink=Just _, ..} =
        ExceptT $ return $ checkoutSymlink fc
    checkoutFile CS.FileContents{symlink=Nothing, ..} = do
        path         <- ExceptT $ return $ Tar.toTarPath False (T.unpack filesPath)
        lazyContents <- runResourceT $ runConduit $ sourceInputStream contents .| sinkLbs

        return $ setMetadata metadata (Tar.fileEntry path lazyContents)

    setMetadata :: CS.Metadata -> Tar.Entry -> Tar.Entry
    setMetadata metadata entry =
        entry { Tar.entryPermissions = CMode (CS.mode metadata),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = fromIntegral (CS.uid metadata),
                                                       Tar.groupId = fromIntegral (CS.gid metadata),
                                                       Tar.ownerName = "",
                                                       Tar.groupName = "" },
                Tar.entryTime = fromIntegral filesMtime }

getGroupId :: (MonadError String m, MonadIO m) => T.Text -> SqlPersistT m (Key Groups)
getGroupId thing = nvraToGroupId (splitFilename thing) >>= \case
    Just gid -> return gid
    Nothing  -> throwError $ "No such group " ++ T.unpack thing

processOneThingToDir :: (MonadError String m, MonadResource m, IsRepo r) => r -> FilePath -> T.Text -> SqlPersistT m ()
processOneThingToDir repo outPath thing = do
    -- Get the group id of the thing
    groupId <- getGroupId thing

    -- Get all of the files associated with the group
    runConduit (groupIdToFiles groupId .| CL.mapM_ (checkoutObjectToDisk repo outPath)) `catchError` handler
 where
    -- Catch errors from processing a single RPM (or whatever), add the name of the thing to the front
    -- of the error message, and re-raise.
    handler err = throwError $ "Error processing " ++ T.unpack thing ++ ": " ++ err

processOneThingToTarEntries :: (MonadError String m, MonadResource m, IsRepo r) => r -> T.Text -> SqlPersistT m [Tar.Entry]
processOneThingToTarEntries repo thing = do
    -- Get the group id of the thing
    groupId <- getGroupId thing

    -- Get all of the files associated with the group, as entries
    runConduit $ groupIdToFiles groupId .| CL.mapM (checkoutObjectToTarEntry repo) .| CL.consume

-- | Check a list of strings to see if any of them are files
-- If it is, read it and insert its contents in its place
expandFileThings :: [String] -> IO [String]
expandFileThings = concatMapM isThingFile
  where
    isThingFile :: String ->  IO [String]
    isThingFile thing = ifM (doesFileExist thing)
                            (lines <$> readFile thing)
                            (return [thing])

-- Turn an RPM filename into a tuple of (name, epoch, version, release, and arch).
splitFilename :: T.Text -> (T.Text, Maybe T.Text, T.Text, T.Text, T.Text)
splitFilename rpm_ = let
    rpm = if ".rpm" `T.isSuffixOf` rpm_ then T.dropEnd 4 rpm_ else rpm_

    (front,  a) = T.breakOnEnd "." rpm
    (front2, r) = T.breakOnEnd "-" $ T.dropWhileEnd (== '.') front
    (front3, v) = T.breakOnEnd "-" $ T.dropWhileEnd (== '-') front2
    (n,      e) = T.breakOn    ":" $ T.dropWhileEnd (== '-') front3
 in
    (T.dropWhile (== ':') n, if e == "" then Nothing else Just e, v, r, a)

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

{-# ANN main ("HLint: ignore Use head" :: String) #-}
main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 4) usage

    let db_path = T.pack (argv !! 0)
    repo <- CS.open (argv !! 1)
    let out_path = argv !! 2
    allThings <- expandFileThings $ drop 3 argv
    let things = map T.pack allThings

    if ".tar" `isSuffixOf` out_path
    then do
        result <- runExceptT $ runResourceT $ processThingsToTar db_path repo out_path things
        whenLeft result $ \e -> do
            print e
            removeFile out_path
    else do
        createDirectoryIfMissing True out_path
        result <- runExceptT $ runResourceT $ processThings db_path repo out_path things
        whenLeft result print
 where
    processThings :: (MonadError String m, MonadBaseControl IO m, MonadResource m, IsRepo a) => T.Text -> a -> FilePath -> [T.Text] -> m ()
    processThings dbPath repo outPath things =
        mapM_ (runSqlite dbPath . processOneThingToDir repo outPath) things

    processThingsToTar :: (MonadError String m, MonadBaseControl IO m, MonadResource m, IsRepo a) => T.Text -> a -> FilePath -> [T.Text] -> m ()
    processThingsToTar dbPath repo outPath things = do
        entries <- concatMapM (runSqlite dbPath . processOneThingToTarEntries repo) things
        liftIO $ writeFile outPath (Tar.write entries)
