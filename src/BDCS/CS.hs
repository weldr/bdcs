{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: BDCS.CS
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Conduit-based interface between BDCS and its underlying content store.

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
import           System.Posix.Files(blockSpecialMode, characterSpecialMode, directoryMode, fileTypeModes, intersectFileModes, namedPipeMode, regularFileMode, symbolicLinkMode)
import           System.Posix.Types(CMode(..), FileMode)

import BDCS.DB
import BDCS.Utils.Either(maybeToEither)

-- | An object in the content store is either a regular file or something else
-- (directory, symlink, etc.) described by the 'Files' metadata.
data Object = SpecialObject                 -- ^ Some non-file object that should be accompanied
                                            -- by a 'Files' record so its metadata can be tracked
            | FileObject BS.ByteString      -- ^ A file object with its contents

-- | Read 'Files' records from a 'Conduit', find the object in the content store, and return the
-- matching 'Object' if found.  An error is thrown if the object does not exist, or if there is
-- any other error interacting with the content store.  In addition, the 'Files' object is also
-- returned as part of the result tuple so its metadata can be used by downstream consumers.
filesToObjectsC :: (MonadError String m, MonadIO m) => ContentStore -> Conduit Files m (Files, Object)
filesToObjectsC repo = awaitForever $ \f@Files{..} ->
    let isRegular = fromIntegral filesMode `intersectFileModes` fileTypeModes == regularFileMode
    in case (isRegular, filesCs_object) of
        -- Not a regular file
        (False, _)         -> yield (f, SpecialObject)
        -- Regular file but no content, probably a %ghost. Just skip it.
        (True, Nothing)    -> return ()
        (True, Just cksum) -> do
            digest <- maybe (throwError "Invalid cs_object") return $ fromByteString (contentStoreDigest repo) cksum
            liftIO (runCsMonad $ fetchByteString repo digest) >>= \case
                Left e    -> throwError (show e)
                Right obj -> yield (f, FileObject obj)

-- | Read tuples from a 'Conduit' and convert each into a 'Codec.Archive.Tar.Entry' suitable for
-- streaming into an archive.  Metadata such as permissions and ownerships will be set correctly.
-- Symlinks and other special non-file things will be handled correctly.  This function is suitable
-- as a downstream consumer of 'filesToObjectsC'.
objectToTarEntry :: (MonadError String m, MonadIO m) => Conduit (Files, Object) m Tar.Entry
objectToTarEntry = awaitForever $ \(f@Files{..}, obj) -> do
    result <- case obj of
            SpecialObject       -> return $ checkoutSpecial f
            FileObject contents -> liftIO . runExceptT $ checkoutFile f contents

    either (\e -> throwError $ "Could not checkout out object " ++ T.unpack filesPath ++ ": " ++ e)
           yield
           result

    objectToTarEntry
 where
    modeToContent :: FileMode -> Either String Tar.EntryContent
    modeToContent mode =
        if | mode == directoryMode        -> Right Tar.Directory
           | mode == namedPipeMode        -> Right Tar.NamedPipe
           -- TODO major/minor
           | mode == characterSpecialMode -> Right $ Tar.CharacterDevice 0 0
           | mode == blockSpecialMode     -> Right $ Tar.BlockDevice 0 0
           | otherwise                    -> Left "Invalid file mode"

    checkoutSpecial :: Files -> Either String Tar.Entry
    checkoutSpecial f@Files{..} = let mode = fromIntegral filesMode `intersectFileModes` fileTypeModes
     in if mode == symbolicLinkMode then
            maybe (Left "Missing symlink target") (checkoutSymlink f) filesTarget
        else do
            path <- Tar.toTarPath True (T.unpack filesPath)
            content <- modeToContent mode
            return $ setMetadata f (Tar.simpleEntry path content)

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
