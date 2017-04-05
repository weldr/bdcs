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

{-# LANGUAGE OverloadedStrings #-}

module BDCS.RPM.Files(mkFiles)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Data.Maybe(fromJust, fromMaybe)
import qualified Data.Text as T
import           Data.Word(Word16, Word32)
import           Database.Esqueleto
import           System.FilePath.Posix((</>))

import BDCS.DB
import BDCS.FileType(getFileType)
import RPM.Tags(Tag, findStringListTag, findTag, tagValue)

type FileTuple = (String, String, Int, String, String, Int, Int, Maybe String)

mkFiles :: MonadIO m => [Tag] -> [(T.Text, Maybe T.Text)] -> SqlPersistT m [Files]
mkFiles rpm checksums =
    mapM mkOneFile (zipFiles rpm)
 where
    mkOneFile :: MonadIO m => FileTuple -> SqlPersistT m Files
    mkOneFile (path, digest, mode, user, group, size, mtime, target) = do
        -- FIXME: This could return Nothing, but only if the database were built wrong.
        -- Is it worth catching that error here and doing... something?
        ty <- fromJust <$> getFileType mode
        let cksum = case lookup (T.pack path) checksums of
                        Just (Just c) -> c
                        _             -> "UNKNOWN"
        return $ Files path digest ty mode user group size mtime target (T.unpack cksum)

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
