{-# LANGUAGE LambdaCase #-}

module BDCS.Files(FileTuple,
                  insertFiles,
                  mkFile,
                  associateFilesWithBuild,
                  associateFilesWithPackage)
 where

import Control.Monad.IO.Class(MonadIO)
import Data.Maybe(fromMaybe)
import Data.Word(Word16, Word32)
import Database.Esqueleto
import System.FilePath.Posix((</>))

import BDCS.DB
import BDCS.FileType(getFileType)
import RPM.Tags(Tag, findStringListTag, findTag, tagValue)

type FileTuple = (String, String, Int, String, String, Int, Int, Maybe String)

insertFiles :: MonadIO m => [Tag] -> SqlPersistT m [Key Files]
insertFiles rpm =
    mapM (insert . mkFile)
         (zipFiles rpm)
 where
    filePaths :: [Tag] -> [String]
    filePaths tags = let
        indexes   = fromMaybe [] $ findTag "DirIndexes" tags >>= \t -> tagValue t :: Maybe [Word32]
        dirnames  = findStringListTag "DirNames" tags
        basenames = findStringListTag "BaseNames" tags
     in
        zipWith (</>) (map (\i -> dirnames !! fromIntegral i) indexes) basenames

    zipFiles :: [Tag] -> [FileTuple]
    zipFiles tags = let
        megazip :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [(a, b, c, d, e, f, g, h)]
        megazip (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) = (a, b, c, d, e, f, g, h) : megazip as bs cs ds es fs gs hs
        megazip _ _ _ _ _ _ _ _ = []

        strToMaybe s = if s == "" then Nothing else Just s

        paths   = filePaths tags
        digests = findStringListTag "FileMD5s" tags
        modes   = fromMaybe [] $ findTag "FileModes" tags     >>= \t -> (tagValue t :: Maybe [Word16]) >>= Just . map fromIntegral
        users   = findStringListTag "FileUserName" tags
        groups  = findStringListTag "FileGroupName" tags
        sizes   = fromMaybe [] $ findTag "FileSizes" tags     >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
        mtimes  = fromMaybe [] $ findTag "FileMTimes" tags    >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
        targets = fromMaybe [] $ findTag "FileLinkTos" tags   >>= \t -> (tagValue t :: Maybe [String]) >>= Just . map strToMaybe
     in
        megazip paths digests modes users groups sizes mtimes targets

mkFile :: FileTuple -> Files
mkFile (path, digest, mode, user, group, size, mtime, target) =
    Files path digest (getFileType mode) mode user group size mtime target

associateFilesWithBuild :: MonadIO m => [Key Files] -> Key Builds -> SqlPersistT m [Key BuildFiles]
associateFilesWithBuild files build =
    mapM (\(fID, bID) -> insert $ BuildFiles bID fID)
         (zip files $ repeat build)

associateFilesWithPackage :: MonadIO m => [Key Files] -> Key KeyVal -> SqlPersistT m [Key FileKeyValues]
associateFilesWithPackage files package =
    mapM (\(fID, pID) -> insert $ FileKeyValues fID pID)
         (zip files $ repeat package)
