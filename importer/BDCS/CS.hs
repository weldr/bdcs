{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BDCS.CS(Object(..),
               filesToObjectsC,
               objectToTarEntry)
 where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Monad.Except(ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy(fromStrict)
import           Data.Conduit(Conduit, awaitForever, yield)
import           Data.ContentStore(ContentStore, contentStoreDigest, fetchByteString, runCsMonad)
import           Data.ContentStore.Digest(fromByteString)
import qualified Data.Text as T
import           System.Posix.Types(CMode(..))

import BDCS.DB
import Utils.Either(maybeToEither)

data Object = DirObject
            | FileObject BS.ByteString

filesToObjectsC :: (MonadError String m, MonadIO m) => ContentStore -> Conduit Files m (Files, Object)
filesToObjectsC repo = awaitForever $ \f@Files{..} -> case filesCs_object of
    -- If we've got a Files row for it, but there's no reference in that row to
    -- an object in the content store, it's a directory.
    -- FIXME:  Is that a valid assumption?  Could there ever be a row without a
    -- reference that is completely invalid?
    Nothing    -> yield (f, DirObject)
    Just cksum -> do
        digest <- maybe (throwError "Invalid cs_object") return $ fromByteString (contentStoreDigest repo) cksum
        liftIO (runCsMonad $ fetchByteString repo digest) >>= \case
            Left e    -> throwError (show e)
            Right obj -> yield (f, FileObject obj)

objectToTarEntry :: (MonadError String m, MonadIO m) => Conduit (Files, Object) m Tar.Entry
objectToTarEntry = awaitForever $ \(f@Files{..}, obj) -> do
    result <- case obj of
            DirObject           -> return $ checkoutDir f
            FileObject contents -> liftIO . runExceptT $ checkoutFile f contents

    either (\e -> throwError $ "Could not checkout out object " ++ T.unpack filesPath ++ ": " ++ e)
           yield
           result

    objectToTarEntry
 where
    checkoutDir :: Files -> Either String Tar.Entry
    checkoutDir f@Files{..} = do
        path <- Tar.toTarPath True (T.unpack filesPath)
        return $ setMetadata f (Tar.directoryEntry path)

    checkoutSymlink :: Files -> T.Text -> Either String Tar.Entry
    checkoutSymlink f@Files{..} target = do
        path'   <- Tar.toTarPath False (T.unpack filesPath)
        target' <- maybeToEither ("Path is too long or contains invalid characters: " ++ T.unpack target)
                                 (Tar.toLinkTarget (T.unpack target))
        return $ setMetadata f (Tar.simpleEntry path' (Tar.SymbolicLink target'))

    checkoutFile :: Files -> BS.ByteString -> ExceptT String IO Tar.Entry
    checkoutFile f@Files{filesTarget=Just target, ..} _ =
        ExceptT $ return $ checkoutSymlink f target
    checkoutFile f@Files{..} contents = do
        path <- ExceptT $ return $ Tar.toTarPath False (T.unpack filesPath)
        return $ setMetadata f (Tar.fileEntry path (fromStrict contents))
    -- TODO?

    setMetadata :: Files -> Tar.Entry -> Tar.Entry
    setMetadata Files{..} entry =
        entry { Tar.entryPermissions = CMode (fromIntegral filesMode),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = 0,
                                                       Tar.groupId = 0,
                                                       Tar.ownerName = T.unpack filesFile_user,
                                                       Tar.groupName = T.unpack filesFile_group },
                Tar.entryTime = fromIntegral filesMtime }
