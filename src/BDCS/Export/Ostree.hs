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
{-# LANGUAGE ScopedTypeVariables #-}

module BDCS.Export.Ostree(ostreeSink)
 where

import           Conduit(Conduit, Consumer, Producer, (.|), bracketP, runConduit, sourceDirectory, yield)
import           Control.Conditional(condM, otherwiseM, whenM)
import           Control.Exception(SomeException, bracket_, catch)
import           Control.Monad(void, when)
import           Control.Monad.Except(MonadError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Resource(MonadResource, runResourceT)
import           Crypto.Hash(SHA256(..), hashInitWith, hashFinalize, hashUpdate)
import qualified Data.ByteString as BS (readFile)
import qualified Data.Conduit.List as CL
import           Data.List(isPrefixOf)
import qualified Data.Text as T
import           System.Directory
import           System.FilePath((</>), takeDirectory, takeFileName)
import           System.IO.Temp(createTempDirectory, withTempDirectory)
import           System.Posix.Files(createSymbolicLink, fileGroup, fileMode, fileOwner, getFileStatus, readSymbolicLink)
import           System.Process(callProcess)
import           Text.Printf(printf)

import           GI.Gio(File, fileNewForPath, noCancellable)
import           GI.OSTree

import qualified BDCS.CS as CS
import           BDCS.DB(Files)
import           BDCS.Export.Directory(directorySink)
import           BDCS.Export.Utils(runHacks)
import           BDCS.Utils.Conduit(awaitWith)

import           Paths_bdcs(getDataFileName)

-- Disable a hint in replaceDirs that just makes thing look confusing
{-# ANN ostreeSink ("HLint: ignore Use ." :: String) #-}

ostreeSink :: (MonadError String m, MonadIO m, MonadResource m) => FilePath -> Consumer (Files, CS.Object) m ()
ostreeSink outPath = do
    -- While it's possible to copy objects from one OstreeRepo to another, we can't create our own objects, meaning
    -- we can't add the dirtree objects we would need to tie all of the files together. So to export to a new
    -- ostree repo, first export to a directory, then import that file to a new repo.
    --
    -- Note that writing and importing a tar file does not work, because ostree chokes on paths with symlinks
    -- (e.g., /lib64/libaudit.so.1)
    dst_repo <- liftIO $ open outPath

    bracketP (createTempDirectory (takeDirectory outPath) "export")
        removePathForcibly
        (\tmpDir -> do
            -- Run the sink to export to a directory
            directorySink tmpDir

            liftIO $ do
                -- Add the standard hacks
                runHacks tmpDir

                -- Compile the locale-archive file
                let localeDir = tmpDir </> "usr" </> "lib" </> "locale"
                whenM (doesFileExist $ localeDir </> "locale-archive.tmpl")
                      (callProcess "chroot" [tmpDir, "/usr/sbin/build-locale-archive"])

                -- Add the kernel and initramfs
                installKernelInitrd tmpDir

                -- Replace /etc/nsswitch.conf with the altfiles version
                getDataFileName "nsswitch-altfiles.conf" >>= readFile >>= writeFile (tmpDir </> "etc" </> "nsswitch.conf")

                -- Remove the fstab stub
                removeFile $ tmpDir </> "etc" </> "fstab"

                -- Move things around how rpm-ostree wants them
                renameDirs tmpDir

                -- Enable some systemd service
                doSystemd tmpDir

                -- Convert /var to a tmpfiles entry
                convertVar tmpDir

                -- Add more tmpfiles entries
                let tmpfilesDir = tmpDir </> "usr" </> "lib" </> "tmpfiles.d"
                getDataFileName "tmpfiles-ostree.conf" >>= readFile >>= writeFile (tmpfilesDir </> "weldr-ostree.conf")

                -- Replace a bunch of top-level directories with symlinks
                replaceDirs tmpDir

                -- Create a /sysroot directory
                createDirectory (tmpDir </> "sysroot")

                -- Replace /usr/local with a symlink for some reason
                removePathForcibly $ tmpDir </> "usr" </> "local"
                createSymbolicLink "../var/usrlocal" $ tmpDir </> "usr" </> "local"

                -- rpm-ostree moves /var/lib/rpm to /usr/share/rpm. We don't have a rpmdb to begin
                -- with, so create an empty one at /usr/share/rpm.
                -- rpmdb treats every path as absolute
                rpmdbDir <- makeAbsolute $ tmpDir </> "usr" </> "share" </> "rpm"
                createDirectoryIfMissing True rpmdbDir
                callProcess "rpmdb" ["--initdb", "--dbpath=" ++ rpmdbDir]

                -- import the directory as a new commit
                void $ withTransaction dst_repo $ \r -> do
                    f <- storeDirectory r tmpDir
                    commit r f "Export commit" Nothing)

    -- Regenerate the summary, necessary for mirroring
    repoRegenerateSummary dst_repo Nothing noCancellable

 where
    convertVar :: FilePath -> IO ()
    convertVar exportDir = do
        -- /var needs to be empty except for a couple of symlinks we add at the end
        -- Convert every directory and symlink we find to a tmpfiles entry. For everything
        -- else, warn and remove.
        let tmpfilesDir = exportDir </> "usr" </> "lib" </> "tmpfiles.d"
        createDirectoryIfMissing True tmpfilesDir

        let varDir = exportDir </> "var"
        writeFile (tmpfilesDir </> "weldr-var.conf") =<<
            unlines <$>
            runResourceT (runConduit $ convertToTmp "/var" varDir .| CL.consume)

    -- basePath is the directory we use for the paths in the tmpfiles lines (e.g., /var/lib)
    -- realPath is the actual path we are traversing (e.g., /tmp/export.XXXX/var/lib)
    convertToTmp :: MonadResource m => FilePath -> FilePath -> Producer m String
    convertToTmp basePath realPath =
        sourceDirectory realPath .| recurseAndEmit
     where
        recurseAndEmit :: MonadResource m => Conduit FilePath m String
        recurseAndEmit = awaitWith $ \path -> do
            let baseFilePath = basePath </> takeFileName path

            -- if it's a directory, recurse into it first
            whenM (liftIO $ doesDirectoryExist path) (convertToTmp baseFilePath path)

            -- Emit a tmpfiles line
            condM [(liftIO $ pathIsSymbolicLink path, yieldLink baseFilePath path),
                   (liftIO $ doesDirectoryExist path, yieldDir  baseFilePath path),
                   -- If not something we can represent as a tmpfile, warn and continue
                   (otherwiseM, liftIO $ putStrLn $ "Warning: Unable to convert " ++ baseFilePath ++ " to a tmpfile")]

            -- Remove it
            liftIO $ removePathForcibly path

            -- Repeat
            recurseAndEmit

        yieldLink :: MonadIO m => FilePath -> FilePath -> Producer m String
        yieldLink baseFilePath realFilePath = do
            target <- liftIO $ readSymbolicLink realFilePath
            yield $ printf "L %s - - - - %s" baseFilePath target

        yieldDir :: MonadIO m => FilePath -> FilePath -> Producer m String
        yieldDir baseDirPath realDirPath = do
            stat <- liftIO $ getFileStatus realDirPath

            -- coerce the stat fields into a type that implements PrintfArg
            let mode = fromIntegral $ fileMode stat :: Integer
            let userId = fromIntegral $ fileOwner stat :: Integer
            let groupId = fromIntegral $ fileGroup stat :: Integer

            yield $ printf "d %s %#o %d %d - -" baseDirPath mode userId groupId

    installKernelInitrd :: FilePath -> IO ()
    installKernelInitrd exportDir = do
        -- The kernel and initramfs need to be named /boot/vmlinuz-<CHECKSUM>
        -- and /boot/initramfs-<CHECKSUM>, where CHECKSUM is the sha256
        -- of the kernel+initramfs.

        let bootDir = exportDir </> "boot"

        -- Find a vmlinuz- file.
        kernelList <- filter ("vmlinuz-" `isPrefixOf`) <$> listDirectory bootDir
        when (null kernelList) (error "No kernel found")
        let kernel = bootDir </> head kernelList
        let kver = drop (length ("vmlinuz-" :: String)) (head kernelList)

        -- Create the initramfs
        let initramfs = bootDir </> "initramfs-" ++ kver
        withTempDirectory exportDir "dracut"
            (\tmpDir -> callProcess "chroot"
                [exportDir,
                 "dracut",
                 "--add", "ostree",
                 "--no-hostonly",
                 "--tmpdir=/" ++ takeFileName tmpDir,
                 "-f", "/boot/" ++ takeFileName initramfs,
                 kver])

        -- Append the checksum to the filenames
        kernelData <- BS.readFile kernel
        initramfsData <- BS.readFile initramfs

        let ctx = hashInitWith SHA256
        let update1 = hashUpdate ctx kernelData
        let update2 = hashUpdate update1 initramfsData
        let digest = show $ hashFinalize update2

        renameFile kernel (kernel ++ "-" ++ digest)
        renameFile initramfs (initramfs ++ "-" ++ digest)

    renameDirs :: FilePath -> IO ()
    renameDirs exportDir = do
        -- ostree mandates /usr/etc, and fails if /etc also exists.
        -- There is an empty /usr/etc owned by filesystem, so get rid of that and move /etc to /usr/etc
        let etcPath = exportDir </> "etc"
        let usrEtcPath = exportDir </> "usr" </> "etc"

        removePathForcibly usrEtcPath
        renameDirectory etcPath usrEtcPath

        -- usr/etc/passwd and usr/etc/group are supposed to be empty (except root and wheel)
        -- the real ones go in /usr/lib/{passwd,group}
        let usrLibPath = exportDir </> "usr" </> "lib"
        renameFile (usrEtcPath </> "passwd") (usrLibPath </> "passwd")
        renameFile (usrEtcPath </> "group")  (usrLibPath </> "group")
        writeFile  (usrEtcPath </> "passwd") "root:x:0:0:root:/root:/bin/bash\n"
        writeFile  (usrEtcPath </> "group")  "root:x:0:\nwheel:x:10:\n"

        -- NB: rpm-ostree also requires that /var/lib/rpm be moved to /usr/share/rpm, but we don't have any
        -- real RPM data to move.

    replaceDirs :: FilePath -> IO ()
    replaceDirs exportDir = do
        -- Clear out anything that's already there.
        -- removeDirectory will fail if not directory exists but is not empty
        mapM_ (\dir -> whenM (doesPathExist dir) (removeDirectory dir))
              (map (exportDir </>) ["home", "media", "mnt", "opt", "root", "srv", "tmp"])

        -- And replace (plus one new one, /ostree)
        createSymbolicLink "var/home"       (exportDir </> "home")
        createSymbolicLink "run/media"      (exportDir </> "media")
        createSymbolicLink "var/mnt"        (exportDir </> "mnt")
        createSymbolicLink "var/opt"        (exportDir </> "opt")
        createSymbolicLink "sysroot/ostree" (exportDir </> "ostree")
        createSymbolicLink "var/roothome"   (exportDir </> "root")
        createSymbolicLink "var/srv"        (exportDir </> "srv")
        createSymbolicLink "sysroot/tmp"    (exportDir </> "tmp")

    doSystemd :: FilePath -> IO ()
    doSystemd exportDir = do
        let systemdDir = exportDir </> "usr" </> "etc" </> "systemd" </> "system"
        createDirectoryIfMissing True systemdDir

        -- Set the default target to multi-user
        createSymbolicLink "/usr/lib/systemd/system/multi-user.target" $ systemdDir </> "default.target"

        -- Add some services that look important
        createDirectoryIfMissing True $ systemdDir </> "getty.target.wants"
        createDirectoryIfMissing True $ systemdDir </> "local-fs.target.wants"

        createSymbolicLink "/usr/lib/systemd/system/getty@.service" $ systemdDir </> "getty.target.wants" </> "getty@tty1.service"
        createSymbolicLink "/usr/lib/systemd/system/ostree-remount.service" $ systemdDir </> "local-fs.target.wants" </> "ostree-remount.service"

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

-- Open the named ostree repo.  If the repo does not already exist, it will first be created.
-- It is created in Z2 mode because that can be modified without being root.
open :: FilePath -> IO Repo
open fp = do
    path <- fileNewForPath fp
    repo <- repoNew path

    doesDirectoryExist fp >>= \case
        True  -> repoOpen repo noCancellable >> return repo
        False -> repoCreate repo RepoModeArchiveZ2 noCancellable >> return repo

-- Given a commit, return the parent of it or Nothing if no parent exists.
parentCommit :: IsRepo a => a -> T.Text -> IO (Maybe T.Text)
parentCommit repo commitSum =
    catch (Just <$> repoResolveRev repo commitSum False)
          (\(_ :: SomeException) -> return Nothing)

-- Same as store, but takes a path to a directory to import
storeDirectory :: IsRepo a => a -> FilePath -> IO File
storeDirectory repo path = do
    importFile <- fileNewForPath path
    mtree <- mutableTreeNew

    repoWriteDirectoryToMtree repo importFile mtree Nothing noCancellable
    repoWriteMtree repo mtree noCancellable

-- Wrap some repo-manipulating function in a transaction, committing it if the function succeeds.
-- Returns the checksum of the commit.
withTransaction :: IsRepo a => a -> (a -> IO b) -> IO b
withTransaction repo fn =
    bracket_ (repoPrepareTransaction repo noCancellable)
             (repoCommitTransaction repo noCancellable)
             (fn repo)