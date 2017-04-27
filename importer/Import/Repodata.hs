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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Import.Repodata(RepoException,
                       loadFromFile,
                       loadFromURL,
                       loadRepoFromFile,
                       loadRepoFromURL)
 where

import           Control.Applicative((<|>))
import           Control.Exception(Exception)
import           Control.Monad.Reader(ReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit((.|), runConduitRes)
import           Data.Data(Typeable)
import           Data.Maybe(listToMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding(encodeUtf8)
import           Network.HTTP.Conduit(path)
import           Network.HTTP.Simple(Request, setRequestPath)
import           System.FilePath((</>), dropDrive, takeDirectory)
import           Text.XML(Document, sinkDoc)
import           Text.XML.Cursor
import           Text.XML.Stream.Parse(def)

import qualified Import.Comps as Comps
import           Import.Conduit(getFromFile, getFromURL, ungzipIfCompressed)
import qualified Import.RPM as RPM
import           Import.State(ImportState(..))

import           BDCS.Exceptions(throwIfNothing)

data RepoException = RepoException
 deriving(Show, Typeable)

instance Exception RepoException

extractLocations :: Document -> [T.Text]
extractLocations doc = let
    cursor = fromDocument doc
 in
    -- Find all <location href=""> elements and return the href's value.  laxElement
    -- means we ignore case and ignore namespacing.  Otherwise we need to take into
    -- account the namespace given in the primary.xml.
    cursor $// laxElement "location"
           >=> hasAttribute "href"
           >=> attribute "href"

-- For a given datatype name, return the first /<root>/data[@type=<type>]/@href
extractType :: Document -> T.Text -> Maybe T.Text
extractType doc dataType = let
    cursor = fromDocument doc
 in
    listToMaybe $ cursor $/ laxElement "data" >=>
                            attributeIs "type" dataType &/
                            laxElement "location" >=>
                            attribute "href"

loadRepoFromURL :: Request -> ReaderT ImportState IO ()
loadRepoFromURL baseRequest = do
    -- Fetch and parse repomd.xml
    repomd <- runConduitRes $ getFromURL (appendPath "repodata/repomd.xml") .| sinkDoc def

    -- Import primary.xml
    let primary = throwIfNothing (extractType repomd "primary") RepoException
    loadFromURL $ appendPath primary

    -- Import comps if it exists
    -- Try group_gz, then group. If neither exists group will be Nothing, which is fine, just skip it
    let group = extractType repomd "group_gz" <|> extractType repomd "group"
    let groupRequest = fmap appendPath group
    case groupRequest of
        Just r  -> Comps.loadFromURL r
        Nothing -> return ()

 where
    -- append a path to the base URL
    appendPath :: T.Text -> Request
    appendPath p = let
        -- If the base request path already ends in a slash, don't add an extra one
        basePath = path baseRequest
        basePathSlash = if C8.last basePath == '/' then basePath else basePath `BS.append` "/"
     in
        setRequestPath (basePathSlash `BS.append` encodeUtf8 p) baseRequest

loadRepoFromFile :: FilePath -> ReaderT ImportState IO ()
loadRepoFromFile baseFile = do
    -- Fetch and parse repomd.xml
    repomd <- runConduitRes $ getFromFile (appendPath "repodata/repomd.xml") .| sinkDoc def

    -- Import primary.xml
    let primary = throwIfNothing (extractType repomd "primary") RepoException
    loadFromFile $ appendPath primary

    -- Import comps if it exists
    -- Try group_gz, then group. If neither exists group will be Nothing, which is fine, just skip it
    let group = extractType repomd "group_gz" <|> extractType repomd "group"
    let groupRequest = fmap appendPath group
    case groupRequest of
        Just r  -> Comps.loadFromFile r
        Nothing -> return ()

 where
    -- append a path to the base URL
    appendPath :: T.Text -> FilePath
    appendPath p = let
        -- use dropDrive to remove the leading / from the path
        p' = dropDrive (T.unpack p)
     in
        baseFile </> p'


loadFromURL :: Request -> ReaderT ImportState IO ()
loadFromURL metadataRequest = do
    let (basePath, _) = BS.breakSubstring "repodata/" (path metadataRequest)
    locations <- map (\p -> metadataRequest { path=BS.concat [basePath, encodeUtf8 p] }) <$> extractLocations <$> runConduitRes (readMetadataPipeline metadataRequest)
    mapM_ RPM.loadFromURL locations
 where
    readMetadataPipeline request = getFromURL request .| ungzipIfCompressed (C8.unpack $ path request) .| sinkDoc def

loadFromFile :: FilePath -> ReaderT ImportState IO ()
loadFromFile metadataPath = do
    locations <- map (\p -> (takeDirectory . takeDirectory) metadataPath </> T.unpack p) <$> extractLocations <$> runConduitRes (readMetadataPipeline metadataPath)
    mapM_ RPM.loadFromFile locations
 where
    readMetadataPipeline p = getFromFile p .| ungzipIfCompressed p .| sinkDoc def
