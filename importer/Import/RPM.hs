-- Copyright (C) 2016-2017 Red Hat, Inc.
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Import.RPM(consume,
                  loadIntoMDDB,
                  loadFromURI,
                  rpmExistsInMDDB)
 where

import           Codec.RPM.Conduit(parseRPMC, payloadContentsC)
import           Codec.RPM.Tags
import           Codec.RPM.Types
import           Control.Conditional(ifM)
import           Control.Exception(evaluate, tryJust)
import           Control.Monad(guard, void)
import           Control.Monad.Except
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader(ReaderT, ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Control(MonadBaseControl)
import           Control.Monad.Trans.Resource(MonadResource)
import qualified Data.ByteString.Char8 as C8
import           Data.CPIO(Entry(..))
import           Data.Conduit((.|), Consumer, ZipConduit(..), await, runConduitRes, yield)
import           Data.Conduit.Combinators(sinkList)
import qualified Data.Conduit.List as CL
import           Data.ContentStore(ContentStore, CsMonad, storeLazyByteStringC)
import           Data.ContentStore.Digest(ObjectDigest)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           Data.Foldable(toList)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
import           Network.URI(URI(..))

import BDCS.Builds(associateBuildWithPackage, insertBuild)
import BDCS.CS(commit, commitContents, store, withTransaction)
import BDCS.DB
import BDCS.Exceptions(DBException(..), isMissingRPMTagException, throwIfNothing)
import BDCS.Files(associateFilesWithBuild, associateFilesWithPackage, insertFiles)
import BDCS.Label.FileLabels(apply)
import BDCS.Packages(insertPackageName)
import BDCS.Projects(insertProject)
import BDCS.Signatures(insertBuildSignatures)
import BDCS.Sources(insertSource)
import BDCS.RPM.Builds(mkBuild)
import BDCS.RPM.Files(mkFiles)
import BDCS.RPM.Groups(createGroup)
import BDCS.RPM.Projects(mkProject)
import BDCS.RPM.Signatures(mkRSASignature, mkSHASignature)
import BDCS.RPM.Sources(mkSource)
import Import.Conduit(getFromURI)
import Import.State(ImportState(..))

#ifdef SCRIPTS
import BDCS.Scripts(insertScript)
import BDCS.RPM.Scripts(mkScripts, mkTriggerScripts)
#endif

buildImported :: MonadIO m => [Tag] ->  SqlPersistT m Bool
buildImported sigs =
    case findStringTag "SHA1Header" sigs of
        Just sha -> do ndx <- select $ from $ \signatures -> do
                              where_ $ signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                       signatures ^. BuildSignaturesSignature_data ==. val (C8.pack sha)
                              return $ signatures ^. BuildSignaturesId
                       return $ not $ null ndx
        Nothing  -> return False

-- A conduit consumer that takes in RPM data and stores its payload into the content store and its header
-- information into the mddb.  The return value is whether or not an import occurred.  This is not the
-- same as success vs. failure, as the import will be skipped if the package already exists in the mddb.
consume :: ContentStore -> FilePath -> Consumer RPM CsMonad Bool
consume repo db = await >>= \case
    Just rpm -> do imported <- lift $ runSqlite (T.pack db) (rpmExistsInMDDB rpm)
                   if imported then return False else unsafeConsume repo db rpm
    Nothing  -> return False

-- Like consume, but does not first check to see if the RPM has previously been imported.  Running
-- this could result in a very confused, incorrect mddb.  It is currently for internal use only,
-- but that might change in the future.  If so, its type should also change to be a Consumer.
unsafeConsume :: ContentStore -> FilePath -> RPM -> Consumer RPM CsMonad Bool
unsafeConsume repo db rpm = do
    -- One source that takes an RPM, extracts its payload, and decompresses it.
    let src       = yield rpm .| payloadContentsC
    -- The first conduit just extracts filenames out of each cpio entry.
        filenames = CL.map (decodeUtf8 . cpioFileName) .| sinkList
    -- The second conduit extracts each file from a cpio entry, stores it in the content store,
    -- and returns its digest.
        digests   = CL.map cpioFileData .| storeLazyByteStringC repo .| sinkList

    -- And then those two conduits run in parallel and the results are packaged up together so
    -- we have filenames and their digests matched up.
    result <- liftIO $ runExceptT $ runConduitRes $ src
                    .| getZipConduit ((,) <$> ZipConduit filenames
                                          <*> ZipConduit digests)

    -- checksums comes back as a ([filename], [digest]).  We need to turn that around
    -- to be [(filename, digest]).
    checksums <- either throwError (return . uncurry zip) result

    lift $ runSqlite (T.pack db) $
        loadIntoMDDB rpm checksums

-- Load the headers from a parsed RPM into the mddb.  The return value is whether or not an import
-- occurred.  This is not the same as success vs. failure, as the import will be skipped if the
-- package already exists in the mddb.
loadIntoMDDB :: (MonadBaseControl IO m, MonadIO m, MonadResource m) => RPM -> [(T.Text, ObjectDigest)] -> SqlPersistT m Bool
loadIntoMDDB rpm checksums =
    ifM (rpmExistsInMDDB rpm)
        (return False)
        (unsafeLoadIntoMDDB rpm checksums)

-- Like loadIntoMDDB, but does not first check to see if the RPM has previously been imported.  Running
-- this could result in a very confused, incorrect mddb.  It is currently for internal use only, but
-- that might change in the future.
unsafeLoadIntoMDDB :: (MonadBaseControl IO m, MonadIO m, MonadResource m) => RPM -> [(T.Text, ObjectDigest)] -> SqlPersistT m Bool
unsafeLoadIntoMDDB RPM{..} checksums = do
    let sigHeaders = headerTags $ head rpmSignatures
    let tagHeaders = headerTags $ head rpmHeaders

    projectId <- insertProject $ mkProject tagHeaders
    sourceId  <- insertSource $ mkSource tagHeaders projectId
    buildId   <- insertBuild $ mkBuild tagHeaders sourceId

    -- Ignore missing tag errors from mkRSASignature, it just means the RPM is unsigned
    -- toList <$> tryJust will convert the result to [BuildSignatures], either containing one record on
    -- success or an empty list on error.
    rsaSignature <- liftIO $ toList <$> tryJust (guard . isMissingRPMTagException) (evaluate $ mkRSASignature sigHeaders buildId)
    void $ insertBuildSignatures (mkSHASignature sigHeaders buildId:rsaSignature)

    pkgNameId <- insertPackageName $ T.pack $ findStringTag "Name" tagHeaders `throwIfNothing` MissingRPMTag "Name"

    files     <- mkFiles tagHeaders checksums
    filesIds  <- insertFiles files

    -- Pair up files with their IDs in the Files table.  Then use this mapping to add all the
    -- various file-based labels to the KeyVal table.
    void $ apply (zip files filesIds)

    void $ associateFilesWithBuild filesIds buildId
    void $ associateFilesWithPackage filesIds pkgNameId
    void $ associateBuildWithPackage buildId pkgNameId

#ifdef SCRIPTS
    -- groups and reqs
    groupId <- createGroup filesIds tagHeaders

    -- scripts - These are here temporarily, just so we can figure out how widely they are
    -- used.  Once we are done, they are going away.
    mapM_ (insertScript groupId) (mkScripts tagHeaders ++ mkTriggerScripts tagHeaders)
#else
    void $ createGroup filesIds tagHeaders
#endif

    return True

loadFromURI :: URI -> ReaderT ImportState IO ()
loadFromURI uri = do
    db <- stDB <$> ask
    repo <- stRepo <$> ask

    -- Ideally, this would all be one big conduit.  However, I can't figure out how to make that work
    -- when parseRPMC returns one type of error (MonadError ParseError) and consume returns some other
    -- type of error (CsError, hiding in CsMonad).  There doesn't appear to be any good way to
    -- transform error types.  Thus, it's exploded out into two distinct steps.
    result <- runExceptT $ runConduitRes $ getFromURI uri .| parseRPMC .| CL.head
    case result of
        Left e           -> liftIO $ putStrLn $ "Error fetching " ++ uriPath uri ++ ": " ++ show e
        Right (Just rpm) -> do
            result' <- liftIO $ runExceptT $ runConduitRes $ yield rpm .| consume repo db
            case result' of
                Right True -> liftIO $ putStrLn $ "Imported " ++ uriPath uri
                Left e     -> liftIO $ putStrLn $ "Error importing " ++ uriPath uri ++ ": " ++ show e
                _          -> return ()

-- Query the MDDB to see if the package has already been imported.  If so, quit now to prevent it
-- from being added to the content store a second time.  Note that loadIntoMDDB also performs this
-- check, but both of these functions are public and therefore both need to prevent duplicate imports.
rpmExistsInMDDB :: MonadIO m => RPM -> SqlPersistT m Bool
rpmExistsInMDDB RPM{..} = do
    let sigHeaders = headerTags $ head rpmSignatures
    buildImported sigHeaders
