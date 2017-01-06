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

module BDCS.Signatures(insertBuildSignatures,
                       mkRSASignature,
                       mkSHASignature)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8(pack)
import           Database.Esqueleto

import BDCS.DB
import RPM.Tags(Tag, findTag, tagValue)

insertBuildSignatures :: MonadIO m => [Tag] -> Key Builds -> SqlPersistT m [Key BuildSignatures]
insertBuildSignatures sigs buildId =
    case (mkRSASignature sigs buildId, mkSHASignature sigs buildId) of
        (Just rsa, Just sha) -> mapM insert [rsa, sha]
        _                    -> return []

mkRSASignature :: [Tag] -> Key Builds -> Maybe BuildSignatures
mkRSASignature tags buildId = do
    rsaSig <- findTag "RSAHeader" tags >>= \t -> tagValue t :: Maybe BS.ByteString
    return $ BuildSignatures buildId "RSA" rsaSig

mkSHASignature :: [Tag] -> Key Builds -> Maybe BuildSignatures
mkSHASignature tags buildId = do
    shaSig <- findTag "SHA1Header" tags >>= \t -> (tagValue t :: Maybe String) >>= Just . pack
    return $ BuildSignatures buildId "SHA1" shaSig
