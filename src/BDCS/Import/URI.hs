-- |
-- Module: BDCS.Import.URI
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Utilities for manipulating 'URI's during import.

module BDCS.Import.URI(appendURI,
                       baseURI,
                       isCompsFile,
                       isPrimaryXMLFile,
                       showURI,
                       uriToPath)
 where

import Data.List(isInfixOf, isSuffixOf)
import Network.URI(URI(..), escapeURIString, isUnescapedInURI,
                   parseURIReference, pathSegments, relativeTo, unEscapeString, uriToString)

-- | Convert a file:// 'URI' to a 'FilePath'.
-- This does not check that the URI is a file:// URI, assumes posix-style paths
uriToPath :: URI -> FilePath
uriToPath uri = unEscapeString $ uriPath uri

-- | Go up one directory in the 'URI'.  For instance:
-- > ghci> let uri = parseURI "file:///path/to/repo/repodata/primary.xml"
-- > ghci> baseURI (fromJust uri)
-- > Just file:///path/to/repo/
baseURI :: URI -> Maybe URI
baseURI uri = let upOne = parseURIReference ".." in
    fmap (`relativeTo` uri) upOne

-- | Append a path to a 'URI'.
appendURI :: URI -> String -> Maybe URI
appendURI base path = let
    -- Escape the path characters and create a URI reference
    relativeURI = parseURIReference $ escapeURIString isUnescapedInURI path
    appendToBase = (`relativeTo` base)
 in
    fmap appendToBase relativeURI

-- | Convert a URI to string with no obfuscation
showURI :: URI -> String
showURI uri = uriToString id uri ""

-- | Does a 'URI' point to a comps.xml file?  This is only really useful when importing RPMs from
-- a comps file, as shipped by RPM-based distributions.
isCompsFile :: URI -> Bool
isCompsFile uri = let path = last (pathSegments uri) in
    "-comps" `isInfixOf` path && (".xml" `isSuffixOf` path || ".xml.gz" `isSuffixOf` path)

-- | Does a 'URI' point to a primary.xml file?  This is only really useful when importing RPMs from
-- a repo in an RPM-based distribution.
isPrimaryXMLFile :: URI -> Bool
isPrimaryXMLFile uri = "primary.xml" `isInfixOf` last (pathSegments uri)
