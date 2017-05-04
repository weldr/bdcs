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

module Import.URI(appendURI,
                  showURI,
                  uriToPath)
 where

import Network.URI(URI(..), escapeURIString, isUnescapedInURI,
                   parseURIReference, relativeTo, unEscapeString, uriToString)

-- convert a file:// URI to a FilePath
-- This does not check that the URI is a file:// URI, assumes posix-style paths
uriToPath :: URI -> FilePath
uriToPath uri = unEscapeString $ uriPath uri

-- append a path to a URI
appendURI :: URI -> String -> Maybe URI
appendURI base path = let
    -- Escape the path characters and create a URI reference
    relativeURI = parseURIReference $ escapeURIString isUnescapedInURI path
    appendToBase = (`relativeTo` base)
 in
    fmap appendToBase relativeURI

-- Convert a URI to string with no obfuscation
showURI :: URI -> String
showURI uri = uriToString id uri ""
