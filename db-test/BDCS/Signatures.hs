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
