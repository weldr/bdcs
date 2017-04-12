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
import           Data.List(zip4)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Data.Word(Word32)
import           Database.Esqueleto
import           System.FilePath.Posix((</>))

import BDCS.DB
import RPM.Tags(Tag, findStringListTag, findTag, tagValue)

type FileTuple = (T.Text, T.Text, T.Text, Int)

mkFiles :: MonadIO m => [Tag] -> [(T.Text, T.Text)] -> SqlPersistT m [Files]
mkFiles rpm checksums =
    mapM mkOneFile (zipFiles rpm)
 where
    mkOneFile :: MonadIO m => FileTuple -> SqlPersistT m Files
    mkOneFile (path, user, group, mtime) = do
        -- FIXME: This could return Nothing, but only if the database were built wrong.
        -- Is it worth catching that error here and doing... something?
        let cksum = fromMaybe "UNKNOWN" (lookup path checksums)
        return $ Files path user group mtime cksum

    filePaths :: [Tag] -> [FilePath]
    filePaths tags = let
        indexes   = fromMaybe [] $ findTag "DirIndexes" tags >>= \t -> tagValue t :: Maybe [Word32]
        dirnames  = findStringListTag "DirNames" tags
        basenames = findStringListTag "BaseNames" tags
     in
        zipWith (</>) (map (\i -> dirnames !! fromIntegral i) indexes) basenames

    zipFiles :: [Tag] -> [FileTuple]
    zipFiles tags = let
        paths   = map T.pack $ filePaths tags
        users   = map T.pack $ findStringListTag "FileUserName" tags
        groups  = map T.pack $ findStringListTag "FileGroupName" tags
        mtimes  = fromMaybe [] $ findTag "FileMTimes" tags    >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
     in
        zip4 paths users groups mtimes
