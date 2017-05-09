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

{-# LANGUAGE OverloadedStrings #-}

module BDCS.RPM.Projects(mkProject)
 where

import           Data.List(elemIndices)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)

import BDCS.DB(Projects(..))
import BDCS.Exceptions(DBException(..), throwIfNothingOtherwise)
import RPM.Tags(Tag, findByteStringTag, findStringTag)

mkProject :: [Tag] -> Projects
mkProject tags = let
    projectName = throwIfNothingOtherwise (findStringTag "SourceRPM" tags)
                                          (MissingRPMTag "SourceRPM")
                                          (T.pack . srpmToName)
    summary     = throwIfNothingOtherwise (findByteStringTag "Summary" tags)
                                          (MissingRPMTag "Summary")
                                          decodeUtf8
    description = throwIfNothingOtherwise (findByteStringTag "Description" tags)
                                          (MissingRPMTag "Description")
                                          decodeUtf8
    homepage    = fmap T.pack (findStringTag "URL" tags)

    -- FIXME:  Where to get this from?
    upstream_vcs = "UPSTREAM_VCS"
 in
    Projects projectName summary description homepage upstream_vcs
 where
    -- the closest to a project name we have is the srpm name, e.g., pykickstart-2.32-1.fc26.src.rpm
    -- This is essentially N-V-R.A.rpm. rpm does not allow hyphens in version of release, and epoch is
    -- not included in the SRPM name, so we can just take everything before the second-to-last hyphen
    -- as the name.
    srpmToName :: String -> String
    srpmToName s = 
        -- Find all the hyphens and take the second to last result
        let nameHyphenIndex = head $ tail $ reverse $ elemIndices '-' s
        in fst $ splitAt nameHyphenIndex s
