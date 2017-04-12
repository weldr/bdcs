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

{-# LANGUAGE RankNTypes #-}

module Import.Conduit(getFromFile,
                      getFromURL,
                      ungzipIfCompressed)
 where

import           Control.Monad.Trans.Resource(MonadResource)
import qualified Data.ByteString as BS
import           Data.Conduit(ConduitM, Producer, awaitForever, yield)
import           Data.Conduit.Binary(sourceFile)
import           Data.Conduit.Zlib(ungzip)
import           Network.HTTP.Simple(Request, getResponseBody, httpSource)
import           System.FilePath(takeExtension)

-- Load data from a given file into a conduit.
getFromFile :: MonadResource m => FilePath -> Producer m BS.ByteString
getFromFile = sourceFile

-- Load data from a given URL into a conduit.
getFromURL :: MonadResource m => Request -> Producer m BS.ByteString
getFromURL request = httpSource request getResponseBody

-- If a conduit is compressed, pass it through ungzip to uncompress it.  Otherwise, pass it
-- through without doing anything.  We can only tell if a conduit is compressed by also being
-- given the path to the thing being processed.
ungzipIfCompressed :: MonadResource m => FilePath -> ConduitM BS.ByteString BS.ByteString m ()
ungzipIfCompressed path | takeExtension path == ".gz" = ungzip
                        | otherwise                   = awaitForever yield
