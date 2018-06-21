{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: BDCS.Import.Repodata
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions for importing RPM packages from a repo into the database

module BDCS.Import.Repodata(RepoException,
                            loadFromURI,
                            loadRepoFromURI)
 where

import           Control.Applicative((<|>))
import           Control.Exception(Exception, throw)
import           Control.Monad(forM_)
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

import           BDCS.Exceptions(throwIfNothing)
import qualified BDCS.Import.Comps as Comps
import           BDCS.Import.Conduit(getFromURI, ungzipIfCompressed)
import qualified BDCS.Import.RPM as RPM
import           BDCS.Import.State(ImportState(..))
import           BDCS.Import.URI(appendURI, baseURI)

-- | An exception type that is thrown when there is a problem accessing a package
-- repository or processing its metadata.
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

-- | Given the 'URI' to the base of some package repository, fetch its repo metadata and load
-- all its RPMs into the MDDB.  This function must be run within the 'ReaderT' monad, which
-- should be given an 'ImportState' record.  This is how importing knows where to store the
-- results.  If the repo metadata data is invalid, a 'RepoException' will be thrown.  Other
-- errors will be printed to the screen.
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
    forM_ groupURI Comps.loadFromURI

 where
    appendOrThrow :: T.Text -> URI
    appendOrThrow path = appendURI (addSlash uri) (T.unpack path) `throwIfNothing` RepoException

-- | Given the 'URI' to a primary.xml file in some package repository, load all its RPMs
-- into the MDDB.  This function must be run within the 'ReaderT' monad, which should be
-- given an 'ImportState' record.  This is how importing knows where to store the results.
-- If the repo metadata data is invalid, a 'RepoException' will be thrown.  Other errors
-- will be printed to the screen.
loadFromURI :: URI -> ReaderT ImportState IO ()
loadFromURI metadataURI = do
    document <- fetchAndParse metadataURI
    let locations = map appendOrThrow $ extractLocations document
    mapM_ RPM.loadFromURI locations
 where
    appendOrThrow :: T.Text -> URI
    appendOrThrow path = case baseURI metadataURI of
        Nothing  -> throw RepoException
        Just uri -> appendURI uri (T.unpack path) `throwIfNothing` RepoException
