-- |
-- Module: BDCS.RPM.Projects
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'Projects' record support for RPM packages.

module BDCS.RPM.Projects(mkProject)
 where

import           Codec.RPM.Tags(Tag, findByteStringTag, findStringTag)
import           Data.List(elemIndices)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)

import BDCS.DB(Projects(..))
import BDCS.Exceptions(DBException(..), throwIfNothing, throwIfNothingOtherwise)

-- | Return a 'Projects' record for the RPM package.
mkProject :: [Tag] -> Projects
mkProject tags = let
    srpmTag     = findStringTag "SourceRPM" tags `throwIfNothing` MissingRPMTag "SourceRPM"
    projectName = throwIfNothingOtherwise (srpmToName srpmTag)
                                          (BadName srpmTag)
                                          T.pack
    summary     = throwIfNothingOtherwise (findByteStringTag "Summary" tags)
                                          (MissingRPMTag "Summary")
                                          decodeUtf8
    description = throwIfNothingOtherwise (findByteStringTag "Description" tags)
                                          (MissingRPMTag "Description")
                                          decodeUtf8
    homepage    = fmap T.pack (findStringTag "URL" tags)
 in
    Projects projectName summary description homepage Nothing
 where
    -- the closest to a project name we have is the srpm name, e.g., pykickstart-2.32-1.fc26.src.rpm
    -- This is essentially N-V-R.A.rpm. rpm does not allow hyphens in version of release, and epoch is
    -- not included in the SRPM name, so we can just take everything before the second-to-last hyphen
    -- as the name.
    srpmToName :: String -> Maybe String
    srpmToName s =
        case reverse (elemIndices '-' s) of
            _:ndx:_ -> Just $ fst $ splitAt ndx s
            _       -> Nothing
