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

module RPM.Internal.Numbers
 where

import           Data.Bits((.|.), shift)
import qualified Data.ByteString as BS
import           Data.Word

asWord8 :: BS.ByteString -> Word8
asWord8 bs = fromIntegral (bs `BS.index` 0)

asWord16 :: BS.ByteString -> Word16
asWord16 bs = fromIntegral (bs `BS.index` 0) `shift` 8 .|.
              fromIntegral (bs `BS.index` 1)

asWord32 :: BS.ByteString -> Word32
asWord32 bs = fromIntegral (bs `BS.index` 0) `shift` 24 .|.
              fromIntegral (bs `BS.index` 1) `shift` 16 .|.
              fromIntegral (bs `BS.index` 2) `shift` 8  .|.
              fromIntegral (bs `BS.index` 3)

asWord64 :: BS.ByteString -> Word64
asWord64 bs = fromIntegral (bs `BS.index` 0) `shift` 56 .|.
              fromIntegral (bs `BS.index` 1) `shift` 48 .|.
              fromIntegral (bs `BS.index` 2) `shift` 40 .|.
              fromIntegral (bs `BS.index` 3) `shift` 32 .|.
              fromIntegral (bs `BS.index` 4) `shift` 24 .|.
              fromIntegral (bs `BS.index` 5) `shift` 16 .|.
              fromIntegral (bs `BS.index` 6) `shift` 8  .|.
              fromIntegral (bs `BS.index` 7)
