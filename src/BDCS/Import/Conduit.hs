{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Import.Conduit
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Utilities for working with 'Conduit's when importing.

module BDCS.Import.Conduit(getFromURI,
                           ungzipIfCompressed)
 where

import           Conduit(Conduit, Producer, (.|), leftover, sourceFile)
import           Control.Monad.Trans.Resource(MonadResource)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.Binary as CB(take)
import           Data.Conduit.Zlib(ungzip)
import           Data.Word(Word8)
import           Network.HTTP.Simple(getResponseBody, httpSource, parseRequest)
import           Network.URI(URI(..))

import           BDCS.Import.URI(showURI, uriToPath)
import           BDCS.Utils.Conduit(identityC)

-- | Load data from a given file: or http: 'URI' into a 'BS.ByteString'.
getFromURI :: MonadResource m => URI -> Producer m BS.ByteString
getFromURI uri@URI{..} | uriScheme == "file:" = sourceFile $ uriToPath uri
                       | otherwise            = do request <- parseRequest $ showURI uri
                                                   httpSource request getResponseBody

-- | If the 'BS.ByteString' in a 'Conduit' is compressed, pass it through ungzip to
-- uncompress it.  Otherwise, pass it through without doing anything.  We determine
-- whether a stream is compressed by looking for the gzip magic bytes at the start
-- of the stream.
ungzipIfCompressed :: MonadResource m => Conduit BS.ByteString m BS.ByteString
ungzipIfCompressed = do
    magic <- CB.take 2
    let nextPipe = if BSL.unpack magic == gzipMagic then ungzip else identityC

    -- send the two bytes we consumed as a leftover, then continue the conduit
    leftover (BSL.toStrict magic) .| nextPipe
    nextPipe
 where
    gzipMagic :: [Word8]
    gzipMagic = [0x1f, 0x8b]
