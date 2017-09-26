{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BDCS.CS(ChecksumMap,
               Object(..),
               Metadata(..),
               FileContents(..),
               commit,
               commitContents,
               commitContentToFile,
               filesToObjectsC,
               load,
               objectToTarEntry,
               open,
               store,
               storeDirectory,
               withTransaction)
 where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Conditional((<&&>), condM, ifM, otherwiseM, whenM)
import           Control.Exception(SomeException, bracket_, catch)
import           Control.Monad(forM_)
import           Control.Monad.Except(ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.State(StateT, lift, modify)
import           Control.Monad.Trans.Resource(runResourceT)
import qualified Data.ByteString as BS
import           Data.Conduit((.|), Conduit, runConduit, yield)
import           Data.Conduit.Binary(sinkLbs)
import           Data.GI.Base.ManagedPtr(unsafeCastTo)
import           Data.Maybe(isNothing)
import qualified Data.Text as T
import           Data.Word(Word32, Word64)
import           GI.Gio
import           GI.OSTree
import           System.Directory(doesDirectoryExist)
import           System.Endian(fromBE32)
import           System.FilePath((</>), isAbsolute, makeRelative, normalise)
import           System.IO(hClose)
import           System.IO.Temp(withSystemTempFile)
import           System.Posix.Files(getSymbolicLinkStatus, modificationTimeHiRes)
import           System.Posix.Types(CMode(..))

import BDCS.DB
import Utils.Conduit(awaitWith, sourceInputStream)
import Utils.Either(maybeToEither)

-- A mapping from a file path to its checksum in an ostree repo.
type ChecksumMap = [(T.Text, T.Text)]

data Object = DirMeta Metadata
            | FileObject FileContents

-- The metadata for a content store object, shared by file and dirmeta
data Metadata = Metadata { uid :: Word32,
                           gid :: Word32,
                           mode :: Word32,
                           size :: Word64,
                           xattrs :: [(BS.ByteString, BS.ByteString)] }

-- metadata + contents for a file
data FileContents = FileContents { metadata :: Metadata,
                                   symlink :: Maybe T.Text,
                                   contents :: Maybe InputStream }

-- Given a commit, return the parent of it or Nothing if no parent exists.
parentCommit :: IsRepo a => a -> T.Text -> IO (Maybe T.Text)
parentCommit repo commitSum =
    catch (Just <$> repoResolveRev repo commitSum False)
          (\(_ :: SomeException) -> return Nothing)

-- Write the commit metadata object to an opened ostree repo, given the results of calling store.  This
-- function also requires a commit subject and optionally a commit body.  The subject and body are
-- visible if you use "ostree log master".  Returns the checksum of the commit.
commit :: IsRepo a => a -> File -> T.Text -> Maybe T.Text -> IO T.Text
commit repo repoFile subject body =
    unsafeCastTo RepoFile repoFile >>= \root -> do
        -- Get the parent, which should always be whatever "master" points to.  If there is no parent
        -- (likely because nothing has been imported into this repo before), just return Nothing.
        -- ostree will know what to do.
        parent <- parentCommit repo "master"
        checksum <- repoWriteCommit repo parent (Just subject) body Nothing root noCancellable
        repoTransactionSetRef repo Nothing "master" (Just checksum)
        return checksum

-- Given an open ostree repo and a checksum to some commit, return a ChecksumMap.  This is useful for
-- creating a mapping in the MDDB from some MDDB object to its content in the ostree store.
commitContents :: IsRepo a => a -> T.Text -> StateT ChecksumMap IO ()
commitContents repo commitSum = do
    (root, _) <- repoReadCommit repo commitSum noCancellable
    file <- fileResolveRelativePath root "/"
    info <- fileQueryInfo file "*" [FileQueryInfoFlagsNofollowSymlinks] noCancellable
    walk file info
 where
    walk :: File -> FileInfo -> StateT ChecksumMap IO ()
    walk f i = lift (fileInfoGetFileType i) >>= \case
        FileTypeDirectory -> do getPathAndChecksum FileTypeDirectory f >>= addPathAndChecksum

                                -- Grab the info for everything in this directory.
                                dirEnum <- fileEnumerateChildren f "*" [FileQueryInfoFlagsNofollowSymlinks] noCancellable
                                childrenInfo <- getAllChildren dirEnum []

                                -- Examine the contents of this directory recursively - this results in all
                                -- the files being added by the other branch of the case, and other directories
                                -- being handled recusrively.  Thus, we do this depth-first.
                                forM_ childrenInfo $ \childInfo -> do
                                    child <- fileInfoGetName childInfo >>= fileGetChild f
                                    walk child childInfo

        ty                -> getPathAndChecksum ty f >>= addPathAndChecksum

    addPathAndChecksum :: (Maybe T.Text, T.Text) -> StateT ChecksumMap IO ()
    addPathAndChecksum (Just p, c) = modify (++ [(p, c)])
    addPathAndChecksum _           = return ()

    getAllChildren :: FileEnumerator -> [FileInfo] -> StateT ChecksumMap IO [FileInfo]
    getAllChildren enum accum =
        fileEnumeratorNextFile enum noCancellable >>= \case
            Just next -> getAllChildren enum (accum ++ [next])
            Nothing   -> return accum

    getPathAndChecksum :: FileType -> File -> StateT ChecksumMap IO (Maybe T.Text, T.Text)
    getPathAndChecksum ty f = lift $ unsafeCastTo RepoFile f >>= \repoFile -> do
        checksum <- case ty of
                        FileTypeDirectory -> do -- this needs to be called before repoFileTreeGetMetadataChecksum to populate the data
                                                repoFileEnsureResolved repoFile
                                                repoFileTreeGetMetadataChecksum repoFile
                        _                 -> repoFileGetChecksum repoFile

        path <- fileGetPath f
        return (fmap T.pack path, checksum)

filesToObjectsC :: (IsRepo a, MonadError String m, MonadIO m) => a -> Conduit Files m (Files, Object)
filesToObjectsC repo = awaitWith $ \f@Files{..} -> case filesCs_object of
    Nothing       -> filesToObjectsC repo
    Just checksum -> do
        object <- load repo checksum
        yield (f, object)
        filesToObjectsC repo

objectToTarEntry :: (MonadError String m, MonadIO m) => Conduit (Files, Object) m Tar.Entry
objectToTarEntry = awaitWith $ \(f@Files{..}, obj) -> do
    result <- case obj of
            DirMeta dirmeta    -> return $ checkoutDir f dirmeta
            FileObject fileObj -> liftIO . runExceptT $ checkoutFile f fileObj

    either (\e -> throwError $ "Could not checkout out object " ++ T.unpack filesPath ++ ": " ++ e)
           yield
           result

    objectToTarEntry
 where
    checkoutDir :: Files -> Metadata -> Either String Tar.Entry
    checkoutDir f@Files{..} metadata@Metadata{..} = do
        path <- Tar.toTarPath True (T.unpack filesPath)
        return $ setMetadata f metadata (Tar.directoryEntry path)

    checkoutSymlink :: Files -> Metadata -> T.Text -> Either String Tar.Entry
    checkoutSymlink f@Files{..} metadata target = do
        path'   <- Tar.toTarPath False (T.unpack filesPath)
        target' <- maybeToEither ("Path is too long or contains invalid characters: " ++ T.unpack target)
                                 (Tar.toLinkTarget (T.unpack target))
        return $ setMetadata f metadata (Tar.simpleEntry path' (Tar.SymbolicLink target'))

    checkoutFile :: Files -> FileContents -> ExceptT String IO Tar.Entry
    checkoutFile f FileContents{symlink=Just target, ..} =
        ExceptT $ return $ checkoutSymlink f metadata target
    checkoutFile f@Files{..} FileContents{symlink=Nothing, contents=Just c, ..} = do
        path         <- ExceptT $ return $ Tar.toTarPath False (T.unpack filesPath)
        lazyContents <- runResourceT $ runConduit $ sourceInputStream c .| sinkLbs

        return $ setMetadata f metadata (Tar.fileEntry path lazyContents)
    -- TODO?
    checkoutFile _ _ = throwError "Unhandled file type"

    setMetadata :: Files -> Metadata -> Tar.Entry -> Tar.Entry
    setMetadata Files{..} metadata entry =
        entry { Tar.entryPermissions = CMode (mode metadata),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = fromIntegral (uid metadata),
                                                       Tar.groupId = fromIntegral (gid metadata),
                                                       Tar.ownerName = "",
                                                       Tar.groupName = "" },
                Tar.entryTime = fromIntegral filesMtime }

-- Open the named ostree repo.  If the repo does not already exist, it will first be created.
-- It is created in Z2 mode because that can be modified without being root.
open :: FilePath -> IO Repo
open fp = do
    path <- fileNewForPath fp
    repo <- repoNew path

    doesDirectoryExist fp >>= \case
        True  -> repoOpen repo noCancellable >> return repo
        False -> repoCreate repo RepoModeArchiveZ2 noCancellable >> return repo

-- Given an open ostree repo and an RPM payload as a ByteString, store that payload's files into the repo.
-- Returns a File (really, a RepoFile) pointing to the root of the archive.
store :: IsRepo a => a -> BS.ByteString -> IO File
store repo content =
    -- We use a temp file here because there's no ostree function that allows reading from a stream
    -- or open file handle or something.  There's also ostree_repo_import_archive_to_mtree but that
    -- wants the archive in a libarchive-specific structure, which would mean more bindings.  This
    -- requires more disk space but is a lot easier.
    withSystemTempFile "archive" $ \tmpFile hFile -> do
        BS.hPut hFile content
        hClose hFile

        archive <- fileNewForPath tmpFile
        mtree   <- mutableTreeNew

        repoWriteArchiveToMtree repo archive mtree Nothing True noCancellable
        repoWriteMtree repo mtree noCancellable

-- Same as store, but takes a path to a directory to import
storeDirectory :: IsRepo a => a -> FilePath -> IO File
storeDirectory repo path = do
    importFile <- fileNewForPath path
    mtree <- mutableTreeNew

    repoWriteDirectoryToMtree repo importFile mtree Nothing noCancellable
    repoWriteMtree repo mtree noCancellable

-- Given an open ostree repo and a checksum, read an object from the content store.
-- The checksums stored in the mddb can be either dirmeta objects, which will contain
-- the metadata for a directory (mode, xattrs), or a file object, which will contain the file
-- metadata plus contents.
-- On error (such as if the object does not exist) a String exception is thrown.
load :: (IsRepo a, MonadError String m, MonadIO m) => a -> T.Text -> m Object
load repo checksum =
    condM [(repoHasObject repo ObjectTypeDirMeta checksum noCancellable, DirMeta <$> loadDir),
           (repoHasObject repo ObjectTypeFile checksum noCancellable,    FileObject <$> loadFile),
           (otherwiseM, throwError $ "No such object " ++ show checksum)]

  where
    loadDir :: (MonadError String m, MonadIO m) => m Metadata
    loadDir =
        -- Load the object from the content store
        -- This will be a variant of type (uuua(ayay)), which is (UID, GID, mode, [(xattr-name, xattr-value)])
        -- UID, GID and mode are all big-endian
        repoLoadVariant repo ObjectTypeDirMeta checksum >>=
            liftIO . variantToDirMeta >>= \case
                Just (uid, gid, mode, xattrs) -> return Metadata {uid=fromBE32 uid,
                                                                  gid=fromBE32 gid,
                                                                  mode=fromBE32 mode,
                                                                  size=0,
                                                                  xattrs=xattrs}
                Nothing -> throwError $ "Error reading dirmeta object from content store: " ++ show checksum

    loadFile :: (MonadError String m, MonadIO m) => m FileContents
    loadFile = do
        (contents, info, xattrsVariant) <- repoLoadFile repo checksum noCancellable

        -- All of the outputs are Maybe, but some situations don't make any sense
        -- We always need a fileinfo, since we at least need the mode out of that to tell what kind of file it is.
        -- If the file is a regular file, contents must be set.
        info' <- maybe (throwError $ "No file info for " ++ show checksum) return info

        whenM (((== FileTypeRegular) <$> fileInfoGetFileType info') <&&> return (isNothing contents))
              (throwError $ "No file contents for " ++ show checksum)

        -- Fetch the useful parts out the fileinfo
        uid <- fileInfoGetAttributeUint32 info' "unix::uid"
        gid <- fileInfoGetAttributeUint32 info' "unix::gid"
        mode <- fileInfoGetAttributeUint32 info' "unix::mode"
        size <- fileInfoGetAttributeUint64 info' "standard::size"

        symlink <- ifM (fileInfoGetIsSymlink info')
                       (fmap Just (fileInfoGetSymlinkTarget info'))
                       (return Nothing)

        -- the xattrs is a GVariant of type a(ayay), i.e., [(ByteString, ByteString)]
        liftIO (variantToXattrs xattrsVariant) >>= \case
            Just xattrs -> return FileContents {metadata=Metadata{uid, gid, mode, size, xattrs}, symlink, contents}
            Nothing -> throwError $ "Error reading xattrs object for " ++ show checksum

    variantToDirMeta :: GVariant -> IO (Maybe (Word32, Word32, Word32, [(BS.ByteString, BS.ByteString)]))
    variantToDirMeta = fromGVariant

    variantToXattrs :: Maybe GVariant -> IO (Maybe [(BS.ByteString, BS.ByteString)])
    variantToXattrs (Just v)  = fromGVariant v
    variantToXattrs Nothing   = return $ Just []

-- Wrap some repo-manipulating function in a transaction, committing it if the function succeeds.
-- Returns the checksum of the commit.
withTransaction :: IsRepo a => a -> (a -> IO b) -> IO b
withTransaction repo fn =
    bracket_ (repoPrepareTransaction repo noCancellable)
             (repoCommitTransaction repo noCancellable)
             (fn repo)

-- | Create a "Files" record for the given path
commitContentToFile :: FilePath         -- ^ A path prefix to use as the root of the file import
                    -> (T.Text, T.Text) -- ^ The path to convert, relative to the prefix, and the content store checksum
                    -> IO Files         -- ^ The resulting record
commitContentToFile prefix (path, checksum) = do
    -- combine the prefix and the path. If the path already starts with a /, remove it
    let normalPrefix = normalise prefix
    let normalPath   = normalise (T.unpack path)
    let relativePath = if isAbsolute normalPath then makeRelative "/" normalPath else normalPath
    let fullPath = normalPrefix </> relativePath

    -- use getSymbolicLinkStatus (lstat) since there may be broken symlinks
    stat <- getSymbolicLinkStatus fullPath
    let mtime = floor $ modificationTimeHiRes stat

    -- TODO user/group

    return $ Files (T.pack ("/" </> relativePath)) "root" "root" mtime (Just checksum)
