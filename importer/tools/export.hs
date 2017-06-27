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
import           Control.Conditional(ifM, whenM)
import           Control.Monad(unless, void, when)
import           Control.Monad.Except(ExceptT(..), MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource, runResourceT)
import           Data.ByteString(ByteString)
import           Data.ByteString.Lazy(writeFile)
import           Data.Conduit((.|), Conduit, Consumer, Producer, await, runConduit, yield)
import           Data.Conduit.Binary(sinkFile, sinkLbs)
import qualified Data.Conduit.List as CL
import           Data.List(inits, isSuffixOf, isPrefixOf, partition)
import qualified Data.Map as Map
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Data.Tree(Forest, Tree(..), unfoldTree)
import           Database.Persist.Sql(SqlPersistT)
import           Database.Persist.Sqlite(runSqlite)
import           Prelude hiding(writeFile)
import           System.Directory(createDirectoryIfMissing, doesFileExist, removeFile, setModificationTime)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           System.Posix.Files(createSymbolicLink, setFileMode)
import           System.Posix.Types(CMode(..))

import           GI.Gio(IsInputStream, inputStreamReadBytes, noCancellable)
import           GI.GLib(bytesGetData, bytesGetSize)
import           GI.OSTree(IsRepo)

import qualified BDCS.CS as CS
import           BDCS.DB
import           BDCS.Files(groupIdToFilesC)
import           BDCS.Groups(nevraToGroupId)
import           BDCS.RPM.Utils(splitFilename)
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

getGroupIdC :: (MonadError String m, MonadBaseControl IO m, MonadIO m) => Conduit T.Text (SqlPersistT m) (Key Groups)
getGroupIdC = await >>= \case
    Nothing    -> return ()
    Just thing -> do
        maybeId <- lift $ nevraToGroupId (splitFilename thing)
        case maybeId of
            Just gid -> yield gid >> getGroupIdC
            Nothing  -> throwError $ "No such group " ++ T.unpack thing

filesToObjectsC :: (IsRepo a, MonadError String m, MonadIO m) => a -> Conduit Files m (Files, CS.Object)
filesToObjectsC repo = await >>= \case
    Nothing        -> return ()
    Just f@Files{..} -> case filesCs_object of
        Nothing       -> filesToObjectsC repo
        Just checksum -> do
            object <- CS.load repo checksum
            yield (f, object)
            filesToObjectsC repo

objectToTarEntry :: (MonadError String m, MonadIO m) => Conduit (Files, CS.Object) m Tar.Entry
objectToTarEntry = await >>= \case
    Nothing                 -> return ()
    Just (f@Files{..}, obj) -> do
        result <- case obj of
                CS.DirMeta dirmeta    -> return $ checkoutDir f dirmeta
                CS.FileObject fileObj -> liftIO . runExceptT $ checkoutFile f fileObj

        either (\e -> throwError $ "Could not checkout out object " ++ T.unpack filesPath ++ ": " ++ e)
               yield
               result

        objectToTarEntry
 where
    checkoutDir :: Files -> CS.Metadata -> Either String Tar.Entry
    checkoutDir f@Files{..} metadata@CS.Metadata{..} = do
        path <- Tar.toTarPath True (T.unpack filesPath)
        return $ setMetadata f metadata (Tar.directoryEntry path)

    checkoutSymlink :: Files -> CS.Metadata -> T.Text -> Either String Tar.Entry
    checkoutSymlink f@Files{..} metadata target = do
        path'   <- Tar.toTarPath False (T.unpack filesPath)
        target' <- maybeToEither ("Path is too long or contains invalid characters: " ++ T.unpack target)
                                 (Tar.toLinkTarget (T.unpack target))
        return $ setMetadata f metadata (Tar.simpleEntry path' (Tar.SymbolicLink target'))

    checkoutFile :: Files -> CS.FileContents -> ExceptT String IO Tar.Entry
    checkoutFile f CS.FileContents{symlink=Just target, ..} =
        ExceptT $ return $ checkoutSymlink f metadata target
    checkoutFile f@Files{..} CS.FileContents{symlink=Nothing, contents=Just c, ..} = do
        path         <- ExceptT $ return $ Tar.toTarPath False (T.unpack filesPath)
        lazyContents <- runResourceT $ runConduit $ sourceInputStream c .| sinkLbs

        return $ setMetadata f metadata (Tar.fileEntry path lazyContents)
    -- TODO?
    checkoutFile _ _ = throwError "Unhandled file type"

    setMetadata :: Files -> CS.Metadata -> Tar.Entry -> Tar.Entry
    setMetadata Files{..} metadata entry =
        entry { Tar.entryPermissions = CMode (CS.mode metadata),
                Tar.entryOwnership   = Tar.Ownership { Tar.ownerId = fromIntegral (CS.uid metadata),
                                                       Tar.groupId = fromIntegral (CS.gid metadata),
                                                       Tar.ownerName = "",
                                                       Tar.groupName = "" },
                Tar.entryTime = fromIntegral filesMtime }

tarSink :: MonadIO m => FilePath -> Consumer Tar.Entry m ()
tarSink out_path = do
    entries <- CL.consume
    liftIO $ writeFile out_path (Tar.write entries)

-- Write to a directory in two passes: first, write the files, creating directories
-- as needed. This way the current user will be able to write files to the new
-- directories and not have to worry about, for instance, /usr/bin being 0555.
-- After the files are done, arrange the directory entries into a tree and
-- apply permissions depth-first, post-order so that we don't lock ourselves
-- out of higher nodes.
directorySink :: MonadIO m => FilePath -> Consumer (Files, CS.Object) m ()
directorySink outPath = do
    directoryPairs <- processFiles .| CL.consume

    -- Arrange the paths into a tree
    let directoryPaths = map (filesPath . fst) directoryPairs
    let directoryTree = foldl addPathToTree (Node "/" []) directoryPaths

    -- Create a map from the path to the (Files, Object) pair
    let directoryMap = foldl (\acc pair@(f, _) -> Map.insert (filesPath f) pair acc) Map.empty directoryPairs

    -- Walk the tree and apply the directory permissions
    liftIO $ walkTreeM_ (\path -> case Map.lookup path directoryMap of
                            Just (f, metadata) -> checkoutDir f metadata
                            -- Warn about directories in the tree but not the map, this means that there
                            -- this is no package owning a parent of something in a recipe, which could
                            -- be a bug in the recipe
                            Nothing -> putStrLn $ "No metadata found for " ++ show path ++
                                                    ", this may be due to a package missing from a recipe."
                        )
                        directoryTree
 where
    walkTreeM_ :: Monad m => (a -> m b) -> Tree a -> m ()
    walkTreeM_ action Node{subForest=[], ..} = void $ action rootLabel
    walkTreeM_ action Node{..} = do
        -- traverse over the children first
        mapM_ (walkTreeM_ action) subForest

        -- Act on this one
        void $ action rootLabel

    -- If it's a file, write the file and continue
    -- If it's a directory, pass it downstream
    processFiles :: MonadIO m => Conduit (Files, CS.Object) m (Files, CS.Metadata)
    processFiles = await >>= \case
        Nothing                         -> return ()
        Just (f, CS.DirMeta dirmeta)    -> yield (f, dirmeta)              >> processFiles
        Just (f, CS.FileObject fileObj) -> liftIO (checkoutFile f fileObj) >> processFiles

    addPathToTree :: Tree T.Text -> T.Text -> Tree T.Text
    -- All paths start with a /, and we already seeded the root of the tree with a / node,
    -- so skip the head of the list returned by splitPath
    addPathToTree tree path = addPathToTree' tree (tail $ splitPath path)
     where
        addPathToTree' :: Tree T.Text -> [T.Text] -> Tree T.Text
        addPathToTree' t [] = t
        addPathToTree' Node{..} p@(x:rest) =
            case findRoot [] subForest x of
                -- There is no node for this path element, so unfold the rest of the path into a new branch
                (Nothing, _)      -> Node rootLabel (unfoldTree pathToBranch p:subForest)
                -- Existing node, replace it with the recursive result
                (Just n, notPath) -> Node rootLabel (addPathToTree' n rest:notPath)

        -- for example, /usr/share/whatever becomes [/, /usr, /usr/share, /usr/share/whatever]
        splitPath :: T.Text -> [T.Text]
        splitPath p = let
            -- split into ["", "usr", "share", "whatever"]
            bases = T.splitOn "/" p

            -- inits will expand into [[], [""], ["", "usr"], ...]
            -- drop the empty list at the front
            pathLists = tail $ inits bases

            -- put the lists of path components back together
            paths = map (T.intercalate "/") pathLists
         in
            -- intercalate leaves the empty path component at the front as "", but it doesn't really matter,
            -- we're going to ignore it anyway.
            paths

        -- helper for unfoldTree. Creates a node for the first path element and returns the
        -- remainder as a single child.
        pathToBranch :: [T.Text] -> (T.Text, [[T.Text]])
        pathToBranch pathList = let rest = tail pathList in
            (head pathList,
             if null rest then [] else [rest])

        -- Kind of like partition, returns (is thing, not thing), but stops at the first thing found
        findRoot :: Eq a => Forest a -> Forest a -> a -> (Maybe (Tree a), [Tree a])
        findRoot acc [] _ = (Nothing, acc)
        findRoot acc (n@Node{..}:rest) thing =
            if thing == rootLabel then
                (Just n, acc ++ rest)
            else
                findRoot (n:acc) rest thing

    checkoutDir :: Files -> CS.Metadata -> IO ()
    checkoutDir f@Files{..} metadata = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        -- create the directory if it isn't there already
        createDirectoryIfMissing True fullPath

        setMetadata f fullPath metadata

    checkoutFile :: Files -> CS.FileContents -> IO ()
    checkoutFile f@Files{..} CS.FileContents{..} = do
        let fullPath = outPath </> dropDrive (T.unpack filesPath)

        -- TODO: set permissions to something not globally readable?
        createDirectoryIfMissing True $ takeDirectory fullPath

        -- Write the data or the symlink, depending
        case (symlink, contents) of
            (Just symlinkTarget, _) -> createSymbolicLink (T.unpack symlinkTarget) fullPath
            (_, Just c)             -> do
                runResourceT $ runConduit $ sourceInputStream c .| sinkFile fullPath
                setMetadata f fullPath metadata
            -- TODO?
            _                       -> return ()

    setMetadata :: Files -> FilePath -> CS.Metadata -> IO ()
    setMetadata Files{..} fullPath CS.Metadata{..} = do
        -- set the mode
        setFileMode fullPath (CMode mode)

        -- set the mtime
        setModificationTime fullPath (posixSecondsToUTCTime $ realToFrac filesMtime)

        -- TODO user, group, xattrs

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

needFilesystem :: IO ()
needFilesystem = do
    printVersion "export"
    putStrLn "ERROR: The tar needs to have the filesystem package included"
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

    let (match, otherThings) = partition (isPrefixOf "filesystem-") allThings
    when (length match < 1) needFilesystem
    let things = map T.pack $ match !! 0 : otherThings

    let (handler, objectSink) = if ".tar" `isSuffixOf` out_path
            then (\e -> print e >> whenM (doesFileExist out_path) (removeFile out_path), objectToTarEntry .| tarSink out_path)
            else (print, directorySink out_path)

    result <- runExceptT $ runSqlite db_path $ runConduit $ CL.sourceList things
        .| getGroupIdC
        .| groupIdToFilesC
        .| filesToObjectsC repo
        .| objectSink

    whenLeft result handler
