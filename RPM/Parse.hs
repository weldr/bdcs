-- Copyright (C) 2016 Red Hat, Inc.
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module RPM.Parse(parseRPM,
                 parseRPMC)
 where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative((<$>))
#endif
import           Control.Monad.Except(MonadError, throwError)
import           Conduit((=$), awaitForever, yield)
import           Data.Attoparsec.Binary
import           Data.Attoparsec.ByteString(Parser, anyWord8, count, take, takeByteString, word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Conduit(Conduit)
import           Data.Conduit.Attoparsec(ParseError(..), conduitParserEither)
import           Data.Maybe(mapMaybe)
import           Prelude hiding(take)

import RPM.Internal.Numbers(asWord32)
import RPM.Tags(Tag, mkTag)
import RPM.Types(Header(..), Lead(..), RPM(..), SectionHeader(..))

parseLead :: Parser Lead
parseLead = do
    -- Verify this is an RPM by checking the first four bytes.
    word32be 0xedabeedb

    rpmMajor <- anyWord8
    rpmMinor <- anyWord8
    rpmType  <- anyWord16be
    rpmArchNum <- anyWord16be
    rpmName <- C.unpack <$> BS.takeWhile (/= 0) <$> take 66
    rpmOSNum <- anyWord16be
    rpmSigType <- anyWord16be

    -- Skip 16 reserved bytes at the end of the lead.
    take 16
    
    return Lead { rpmMajor,
                  rpmMinor,
                  rpmType,
                  rpmArchNum,
                  rpmName,
                  rpmOSNum,
                  rpmSigType }

parseSectionHeader :: Parser SectionHeader
parseSectionHeader = do
    -- Verify this is a header section header by checking the first three bytes.
    word8 0x8e >> word8 0xad >> word8 0xe8

    sectionVersion <- anyWord8
    -- Skip four reserved bytes.
    take 4
    sectionCount <- anyWord32be
    sectionSize <- anyWord32be

    return SectionHeader { sectionVersion,
                           sectionCount,
                           sectionSize }

parseOneTag :: C.ByteString -> C.ByteString -> Maybe Tag
parseOneTag store bs = let
    tag = fromIntegral . asWord32 $ BS.take 4 bs
    ty  = fromIntegral . asWord32 $ BS.take 4 (BS.drop 4 bs)
    off = fromIntegral . asWord32 $ BS.take 4 (BS.drop 8 bs)
    cnt = fromIntegral . asWord32 $ BS.take 4 (BS.drop 12 bs)
 in
    mkTag store tag ty off cnt

parseSection :: Parser Header
parseSection = do
    headerSectionHeader <- parseSectionHeader
    -- Grab the tags as a list of bytestrings.  We need the store before we can process the tags, as
    -- that's where all the values for the tags are kept.  However, grabbing each individual tag here
    -- makes it a lot easier to process them later.
    rawTags <- count (fromIntegral $ sectionCount headerSectionHeader) (take 16)
    headerStore <- take (fromIntegral $ sectionSize headerSectionHeader)

    -- Now that we've got the store, process each tag by looking up its values in the store.
    let headerTags = mapMaybe (parseOneTag headerStore) rawTags

    return Header { headerSectionHeader,
                    headerTags,
                    headerStore }

parseRPM :: Parser RPM
parseRPM = do
    -- First comes the (mostly useless) lead.
    rpmLead <- parseLead
    -- Then comes the signature, which is like a regular section except it's also padded.
    sig <- parseSection
    take (signaturePadding sig)
    -- And then comes the real header.  There could be several, but for now there's only ever one.
    hdr <- parseSection
    rpmArchive <- takeByteString
    return RPM { rpmLead,
                 rpmSignatures=[sig],
                 rpmHeaders=[hdr],
                 rpmArchive }
 where
    signaturePadding :: Header -> Int
    signaturePadding hdr = let
        remainder = (sectionSize . headerSectionHeader) hdr `mod` 8
     in
        if remainder > 0 then fromIntegral $ 8 - remainder else 0

-- Like parseRPM, but puts the resulting RPM into a Conduit.
parseRPMC :: (MonadError String m) => Conduit C.ByteString m RPM
parseRPMC =
    conduitParserEither parseRPM =$ consumer
 where
    consumer = awaitForever $ \case
        Left err       -> throwError $ errorMessage err
        Right (_, rpm) -> yield rpm
