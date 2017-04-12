{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BDCS.CS(commit,
               commitContents,
               open,
               store,
               withTransaction)
 where

import           Control.Exception(SomeException, bracket_, catch)
import           Control.Monad(forM_, when)
import           Control.Monad.State(StateT, lift, modify)
import qualified Data.ByteString as BS
import           Data.GI.Base.ManagedPtr(unsafeCastTo)
import           Data.Maybe(fromJust, isJust)
import qualified Data.Text as T
import           GI.Gio
import           GI.OSTree
import           System.Directory(doesDirectoryExist)
import           System.IO(hClose)
import           System.IO.Temp(withSystemTempFile)

-- Write the commit metadata object to an opened ostree repo, given the results of calling store.  This
-- function also requires a commit subject and optionally a commit body.  The subject and body are
-- visible if you use "ostree log master".  Returns the checksum of the commit.
commit :: IsRepo a => a -> File -> T.Text -> Maybe T.Text -> IO T.Text
commit repo repoFile subject body =
    unsafeCastTo RepoFile repoFile >>= \root -> do
        -- Get the parent, which should always be whatever "master" points to.  If there is no parent
        -- (likely because nothing has been imported into this repo before), just return Nothing.
        -- ostree will know what to do.
        parent <- catch (Just <$> repoResolveRev repo "master" False)
                        (\(_ :: SomeException) -> return Nothing)

        checksum <- repoWriteCommit repo parent (Just subject) body Nothing root noCancellable
        repoTransactionSetRef repo Nothing "master" checksum
        return checksum

-- Given an open ostree repo and a checksum to some commit, return an association list mapping a file
-- path to its checksum.  This is useful for creating a mapping in the MDDB from the MDDB to the
-- ostree content store.
commitContents :: IsRepo a => a -> T.Text -> StateT [(T.Text, T.Text)] IO ()
commitContents repo commit = do
    (root, _) <- repoReadCommit repo commit noCancellable
    file <- fileResolveRelativePath root "/"
    info <- fileQueryInfo file "*" [FileQueryInfoFlagsNofollowSymlinks] noCancellable
    walk file info
 where
    walk :: File -> FileInfo -> StateT [(T.Text, T.Text)] IO ()
    walk f i = do
        ty <- lift $ fileInfoGetFileType i
        case ty of
            FileTypeDirectory -> do (p, c) <- lift $ unsafeCastTo RepoFile f >>= \repoFile -> do
                                        -- this needs to be called before repoFileTreeGetContentsChecksum to populate the data
                                        repoFileEnsureResolved repoFile

                                        checksum <- repoFileTreeGetContentsChecksum repoFile
                                        path <- fileGetPath f
                                        return (fmap T.pack path, checksum)

                                    -- Add the name and checksum of this directory.
                                    when (isJust p) $
                                        modify (++ [(fromJust p, c)])

                                    -- Grab the info for everything in this directory.
                                    dirEnum <- fileEnumerateChildren f "*" [FileQueryInfoFlagsNofollowSymlinks] noCancellable
                                    childrenInfo <- getAllChildren dirEnum []

                                    -- Examine the contents of this directory recursively - this results in all
                                    -- the files being added by the other branch of the case, and other directories
                                    -- being handled recusrively.  Thus, we do this depth-first.
                                    forM_ childrenInfo $ \childInfo -> do
                                        child <- fileInfoGetName childInfo >>= fileGetChild f
                                        walk child childInfo

            _                 -> do (p, c) <- lift $ unsafeCastTo RepoFile f >>= \repoFile -> do
                                        checksum <- repoFileGetChecksum repoFile
                                        path <- fileGetPath f
                                        return (fmap T.pack path, checksum)

                                    when (isJust p) $
                                        modify (++ [(fromJust p, c)])

    getAllChildren :: FileEnumerator -> [FileInfo] -> StateT [(T.Text, T.Text)] IO [FileInfo]
    getAllChildren enum accum =
        fileEnumeratorNextFile enum noCancellable >>= \case
            Just next -> getAllChildren enum (accum ++ [next])
            Nothing   -> return accum

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

-- Wrap some repo-manipulating function in a transaction, committing it if the function succeeds.
-- Returns the checksum of the commit.
withTransaction :: IsRepo a => a -> (a -> IO b) -> IO b
withTransaction repo fn =
    bracket_ (repoPrepareTransaction repo noCancellable)
             (repoCommitTransaction repo noCancellable)
             (fn repo)
