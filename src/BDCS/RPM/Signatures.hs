{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.RPM.Signatures
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'BuildSignatures' record support for RPM packages.

module BDCS.RPM.Signatures(mkRSASignature,
                           mkSHASignature)
 where

import           Codec.RPM.Tags(Tag, findTag, tagValue)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8(pack)
import           Database.Esqueleto

import BDCS.DB(Builds, BuildSignatures(..))
import BDCS.Exceptions(DBException(..), throwIfNothing)

-- | Return a RSA 'BuildSignature'
--
-- Can throw 'MissingRPMTag'
mkRSASignature :: [Tag] -> Key Builds -> BuildSignatures
mkRSASignature tags buildId = let
    rsaSig = getRSASig `throwIfNothing` MissingRPMTag "RSAHeader"
 in
    BuildSignatures buildId "RSA" rsaSig
 where
    getRSASig = findTag "RSAHeader" tags >>= \t -> tagValue t :: Maybe BS.ByteString

-- | Return a SHA1 'BuildSignature'
--
-- Can throw 'MissingRPMTag'
mkSHASignature :: [Tag] -> Key Builds -> BuildSignatures
mkSHASignature tags buildId = let
    shaSig = getSHASig `throwIfNothing` MissingRPMTag "SHA1Header"
 in
    BuildSignatures buildId "SHA1" shaSig
 where
    getSHASig = findTag "SHA1Header" tags >>= \t -> (tagValue t :: Maybe String) >>= Just . pack
