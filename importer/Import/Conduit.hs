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
{-# LANGUAGE RecordWildCards #-}

module Import.Conduit(getFromURI,
                      ungzipIfCompressed)
 where

import           Conduit(Conduit, Producer, (.|), leftover, mapC, sourceFile)
import           Control.Monad.Trans.Resource(MonadResource)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.Binary as CB(take)
import           Data.Conduit.Zlib(ungzip)
import           Data.Word(Word8)
import           Network.HTTP.Simple(getResponseBody, httpSource, parseRequest)
import           Network.URI(URI(..))

import           Import.URI(showURI, uriToPath)

-- Load data from a given file: or http: URL
getFromURI :: MonadResource m => URI -> Producer m BS.ByteString
getFromURI uri@URI{..} =
    if uriScheme == "file:" then
        sourceFile $ uriToPath uri
    else do
        request <- parseRequest $ showURI uri
        httpSource request getResponseBody

-- If a conduit is compressed, pass it through ungzip to uncompress it.  Otherwise, pass it
-- through without doing anything. Determine whether a stream is compressed by looking for
-- the gzip magic bytes at the start of the stream.
ungzipIfCompressed :: MonadResource m => Conduit BS.ByteString m BS.ByteString
ungzipIfCompressed = do
    magic <- CB.take 2
    let nextPipe = if BSL.unpack magic == gzipMagic then ungzip else identityC

    -- send the two bytes we consumed as a leftover, then continue the conduit
    leftover (BSL.toStrict magic) .| nextPipe
    nextPipe
 where
    identityC :: Monad m => Conduit a m a
    identityC = mapC id

    gzipMagic :: [Word8]
    gzipMagic = [0x1f, 0x8b]
