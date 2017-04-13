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

import           Control.Monad(unless, when)
import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, runResourceT)
import           Data.ByteString(ByteString)
import           Data.Conduit((.|), Producer, runConduit, yield)
import           Data.Conduit.Binary(sinkFile)
import qualified Data.Conduit.List as CL
import           Data.Maybe(fromMaybe)
import           Data.Text(Text, pack, unpack)
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           System.Directory(createDirectoryIfMissing, createFileLink, setModificationTime)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(setFileMode)
import           System.Posix.Types(CMode(..))

import GI.Gio    hiding(on)
import GI.GLib   hiding(String, on)
import GI.OSTree hiding(on)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(groupIdToFiles)
import           BDCS.Groups(nameToGroupId)

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

checkoutObject :: (MonadError String m, MonadIO m, IsRepo a) => a -> FilePath -> Files -> m ()
checkoutObject repo outPath Files{..} =
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
            Just symlinkTarget -> createFileLink (unpack symlinkTarget) fullPath
            Nothing -> runResourceT $ runConduit $ sourceInputStream contents .| sinkFile fullPath

        setMetadata fullPath metadata

    setMetadata :: FilePath -> CS.Metadata -> IO ()
    setMetadata fullPath CS.Metadata{..} = do
        -- set the mode
        setFileMode fullPath (CMode mode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs

processOneThing :: (MonadError String m, MonadResource m, IsRepo r) => r -> FilePath -> Text -> SqlPersistT m ()
processOneThing repo outPath thing = do
    -- Get the group id of the thing
    groupId <- nameToGroupId thing >>= \case
        Just gid -> return gid
        Nothing  -> throwError $ "No such group " ++ unpack thing

    -- Get all of the files associated with the group
    runConduit $ groupIdToFiles groupId .| CL.mapM_ (checkoutObject repo outPath)

usage :: IO ()
usage = do
    putStrLn "Usage: export metadata.db repo outdir thing [thing ...]"
    putStrLn "thing can be:"
    putStrLn "\t* The name of an RPM"
    -- TODO group id?
    exitFailure

main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 4) usage

    let db_path = pack (argv !! 0)
    repo <- CS.open (argv !! 1)
    let out_path = argv !! 2
    let things = map pack $ drop 3 argv

    createDirectoryIfMissing True out_path

    result <- runExceptT $ runResourceT $ processThings db_path repo out_path things

    case result of
        Left e  -> print e
        Right _ -> return ()
 where
    processThings :: (MonadError String m, MonadBaseControl IO m, MonadResource m, IsRepo a) => Text -> a -> FilePath -> [Text] -> m ()
    processThings dbPath repo outPath things = runSqlite dbPath $
        mapM_ (processOneThing repo outPath) things
