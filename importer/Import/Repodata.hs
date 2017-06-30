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
                       loadFromURI,
                       loadRepoFromURI)
 where

import           Control.Applicative((<|>))
import           Control.Exception(Exception)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(ReaderT)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadThrow)
import           Data.Conduit((.|), runConduitRes)
import           Data.Data(Typeable)
import           Data.Maybe(listToMaybe)
import qualified Data.Text as T
import           Network.URI(URI(..))
import           Text.XML(Document, sinkDoc)
import           Text.XML.Cursor
import           Text.XML.Stream.Parse(def)

import qualified Import.Comps as Comps
import           Import.Conduit(getFromURI, ungzipIfCompressed)
import qualified Import.RPM as RPM
import           Import.State(ImportState(..))
import           Import.URI(appendURI, baseURI)

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

-- fetch and parse an XML document
fetchAndParse :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) => URI -> m Document
fetchAndParse uri = runConduitRes $ getFromURI uri .| ungzipIfCompressed .| sinkDoc def

addSlash :: URI -> URI
addSlash u = let
      path = uriPath u
  in
      if last path /= '/' then
        u { uriPath = path ++ "/" }
      else
        u

loadRepoFromURI :: URI -> ReaderT ImportState IO ()
loadRepoFromURI uri = do
    -- Fetch and parse repomd.xml
    repomd <- fetchAndParse (appendOrThrow "repodata/repomd.xml")

    -- Import primary.xml
    let primary = extractType repomd "primary" `throwIfNothing` RepoException
    loadFromURI $ appendOrThrow primary

    -- Import comps if it exists
    -- Try group_gz, then group. If neither exists group will be Nothing, which is fine, just skip it
    let group = extractType repomd "group_gz" <|> extractType repomd "group"
    let groupURI = fmap appendOrThrow group
    case groupURI of
        Just u -> Comps.loadFromURI u
        Nothing -> return ()

 where
    appendOrThrow :: T.Text -> URI
    appendOrThrow path = appendURI (addSlash uri) (T.unpack path) `throwIfNothing` RepoException

loadFromURI :: URI -> ReaderT ImportState IO ()
loadFromURI metadataURI = do
    document <- fetchAndParse metadataURI
    let locations = map appendOrThrow $ extractLocations document
    mapM_ RPM.loadFromURI locations
 where
    appendOrThrow :: T.Text -> URI
    appendOrThrow path = appendURI (baseURI metadataURI) (T.unpack path) `throwIfNothing` RepoException
