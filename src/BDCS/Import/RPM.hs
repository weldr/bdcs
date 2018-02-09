{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Import.RPM
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions for importing individual RPM packages into the database

module BDCS.Import.RPM(consume,
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
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Reader(ReaderT, ask)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Control(MonadBaseControl)
import           Control.Monad.Trans.Resource(MonadResource, MonadThrow)
import qualified Data.ByteString.Char8 as C8
import           Data.CPIO(Entry(..))
import           Data.Conduit((.|), Conduit, Consumer, ZipConduit(..), await, awaitForever, mapOutput, runConduit, runConduitRes, transPipe, yield)
import           Data.Conduit.Combinators(sinkList)
import qualified Data.Conduit.List as CL
import           Data.ContentStore(ContentStore, CsError(..), runCsMonad, storeLazyByteStringC)
import           Data.ContentStore.Digest(ObjectDigest)
import           Database.Esqueleto
import           Data.Foldable(toList)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
import           Network.URI(URI(..))
import           System.Posix.Files(fileTypeModes, intersectFileModes, regularFileMode)

import BDCS.Builds(associateBuildWithPackage, insertBuild)
import BDCS.DB
import BDCS.Exceptions(DBException(..), isMissingRPMTagException, throwIfNothing)
import BDCS.Files(associateFilesWithBuild, associateFilesWithPackage, insertFiles)
import BDCS.Import.Conduit(getFromURI)
import BDCS.Import.State(ImportState(..))
import BDCS.Label.FileLabels(apply)
import BDCS.Packages(insertPackageName)
import BDCS.Projects(insertProject)
import BDCS.RPM.Builds(mkBuild)
import BDCS.RPM.Files(mkFiles)
import BDCS.RPM.Groups(createGroup)
import BDCS.RPM.Projects(mkProject)
import BDCS.RPM.Signatures(mkRSASignature, mkSHASignature)
import BDCS.RPM.Sources(mkSource)
import BDCS.Signatures(insertBuildSignatures)
import BDCS.Sources(insertSource)
import BDCS.Utils.Error(mapError)

#ifdef SCRIPTS
import BDCS.RPM.Scripts(mkScripts, mkTriggerScripts)
import BDCS.Scripts(insertScript)
#endif

{-# ANN buildImported ("HLint: ignore Use ." :: String) #-}

buildImported :: MonadResource m => [Tag] ->  SqlPersistT m Bool
buildImported sigs =
    case findStringTag "SHA1Header" sigs of
        Just sha -> do ndx <- select $ from $ \signatures -> do
                              where_ $ signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                       signatures ^. BuildSignaturesSignature_data ==. val (C8.pack sha)
                              return $ signatures ^. BuildSignaturesId
                       return $ not $ null ndx
        Nothing  -> return False

-- | A conduit consumer that takes in RPM data and stores its payload into the content store and its header
-- information into the mddb.  The return value is whether or not an import occurred.  This is not the
-- same as success vs. failure.  On failures, a 'CsError' will be thrown.  If the package already exists
-- in the database, this function will return False.
consume :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadError CsError m) => ContentStore -> FilePath -> Consumer RPM m Bool
consume repo db = await >>= \case
    Just rpm ->
        lift (runExceptT $ checkAndRunSqlite (T.pack db) (rpmExistsInMDDB rpm)) >>= \case
            Left e      -> throwError (CsError $ show e)
            Right True  -> return False
            Right False -> unsafeConsume repo db rpm
    Nothing  -> return False

-- Like consume, but does not first check to see if the RPM has previously been imported.  Running
-- this could result in a very confused, incorrect mddb.  It is currently for internal use only,
-- but that might change in the future.  If so, its type should also change to be a Consumer.
unsafeConsume :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadError CsError m) => ContentStore -> FilePath -> RPM -> Consumer RPM m Bool
unsafeConsume repo db rpm = do
    -- One source that takes an RPM, extracts its payload, and decompresses it.
    let src       = yield rpm .| payloadContentsC
    -- The first conduit just extracts filenames out of each cpio entry.  cpio puts a leading . on
    -- each filename, but the RPM headers do not have that.  Thus, we trim it out so the paths look
    -- the same.
        filenames = CL.map (T.dropWhile (== '.') . decodeUtf8 . cpioFileName) .| sinkList
    -- The second conduit extracts each file from a cpio entry, stores it in the content store,
    -- and returns its digest.
        digests = maybeStore .| sinkList

    -- And then those two conduits run in parallel and the results are packaged up together so
    -- we have filenames and their digests matched up.
    result <- liftIO $ runCsMonad $ runConduit $ src
                    .| getZipConduit ((,) <$> ZipConduit filenames
                                          <*> ZipConduit digests)

    -- checksums comes back as a ([filename], [digest]).  We need to turn that around
    -- to be [(filename, digest]).
    checksums <- either throwError (return . uncurry zip) result

    lift (runExceptT $ checkAndRunSqlite (T.pack db) (loadIntoMDDB rpm checksums)) >>= \case
        Left e  -> throwError (CsError $ show e)
        Right v -> return v
 where
    maybeStore :: (MonadResource m, MonadError CsError m) => Conduit Entry m (Maybe ObjectDigest)
    maybeStore = awaitForever $ \Entry{..} ->
        -- Only store regular files in the content store
        -- Checking the type is more complicated then you'd think it should be, because
        -- the type mode is more than just one bit. e.g., regular == 100000, symlink == 120000
        if fromIntegral cpioMode `intersectFileModes` fileTypeModes == regularFileMode then
            mapOutput Just $ yield cpioFileData .| storeLazyByteStringC repo
        else
            yield Nothing

-- | Load the headers from a parsed RPM into the MDDB.  The return value is whether or not an import
-- occurred.  This is not the same as success vs. failure.  If the package already exists in the
-- database, this function will return False.
loadIntoMDDB :: (MonadBaseControl IO m, MonadResource m) => RPM -> [(T.Text, Maybe ObjectDigest)] -> SqlPersistT m Bool
loadIntoMDDB rpm checksums =
    ifM (rpmExistsInMDDB rpm)
        (return False)
        (unsafeLoadIntoMDDB rpm checksums)

-- Like loadIntoMDDB, but does not first check to see if the RPM has previously been imported.  Running
-- this could result in a very confused, incorrect mddb.  It is currently for internal use only, but
-- that might change in the future.
unsafeLoadIntoMDDB :: (MonadBaseControl IO m, MonadResource m) => RPM -> [(T.Text, Maybe ObjectDigest)] -> SqlPersistT m Bool
unsafeLoadIntoMDDB RPM{rpmSignatures=[], ..} _                                             = return False
unsafeLoadIntoMDDB RPM{rpmHeaders=[], ..}    _                                             = return False
unsafeLoadIntoMDDB RPM{rpmSignatures=fstSignature:_, rpmHeaders=fstHeader:_, ..} checksums = do
    let sigHeaders = headerTags fstSignature
    let tagHeaders = headerTags fstHeader

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

-- | Fetch an RPM from a given 'URI' and load it into the MDDB.  This function must be
-- run within the 'ReaderT' monad, which should be given an 'ImportState' record.  This
-- is how importing knows where to store the results.  Errors will be printed to the
-- screen.
loadFromURI :: URI -> ReaderT ImportState IO ()
loadFromURI uri = do
    db <- stDB <$> ask
    repo <- stRepo <$> ask

    result <- runExceptT $ runConduitRes $
           getFromURI uri
        .| transPipe (mapError showParseError) parseRPMC
        .| transPipe (mapError showCsError) (consume repo db)

    case result of
        Left e     -> liftIO $ putStrLn e
        Right True -> liftIO $ putStrLn $ "Imported " ++ uriPath uri
        _          -> return ()

 where
    showParseError e = "Error fetching " ++ uriPath uri ++ ": " ++ show e
    showCsError    e = "Error importing " ++ uriPath uri ++ ": " ++ show e

-- | Query the MDDB to see if the package has already been imported.  If so, quit now to prevent it
-- from being added to the content store a second time.  Note that 'loadIntoMDDB' also performs this
-- check, but both of these functions are public and therefore both need to prevent duplicate imports.
rpmExistsInMDDB :: MonadResource m => RPM -> SqlPersistT m Bool
rpmExistsInMDDB RPM{rpmSignatures=[], ..}   = return False
rpmExistsInMDDB RPM{rpmSignatures=hd:_, ..} = do
    let sigHeaders = headerTags hd
    buildImported sigHeaders
