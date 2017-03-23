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

module BDCS.RPM.Signatures(mkRSASignature,
                           mkSHASignature)
 where

import qualified Data.ByteString as BS
import           Data.ByteString.Char8(pack)
import           Database.Esqueleto

import BDCS.DB(Builds, BuildSignatures(..))
import BDCS.Exceptions(DBException(..), throwIfNothing)
import RPM.Tags(Tag, findTag, tagValue)

mkRSASignature :: [Tag] -> Key Builds -> BuildSignatures
mkRSASignature tags buildId = let
    rsaSig = getRSASig `throwIfNothing` MissingRPMTag "RSAHeader"
 in
    BuildSignatures buildId "RSA" rsaSig
 where
    getRSASig = findTag "RSAHeader" tags >>= \t -> tagValue t :: Maybe BS.ByteString

mkSHASignature :: [Tag] -> Key Builds -> BuildSignatures
mkSHASignature tags buildId = let
    shaSig = getSHASig `throwIfNothing` MissingRPMTag "SHA1Header"
 in
    BuildSignatures buildId "SHA1" shaSig
 where
    getSHASig = findTag "SHA1Header" tags >>= \t -> (tagValue t :: Maybe String) >>= Just . pack
