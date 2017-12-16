{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.RPM.Sources
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'Sources' record support for RPM packages.

module BDCS.RPM.Sources(mkSource)
 where

import           Codec.RPM.Tags(Tag, findStringTag)
import           Database.Esqueleto(Key)
import qualified Data.Text as T

import BDCS.DB(Projects, Sources(..))
import BDCS.Exceptions(DBException(..), throwIfNothing)

-- | Return a 'Sources' record.
--
-- Can throw 'MissingRPMTag'
mkSource :: [Tag] -> Key Projects -> Sources
mkSource tags projectId = let
    license = T.pack $ findStringTag "License" tags `throwIfNothing` MissingRPMTag "License"
    version = T.pack $ findStringTag "Version" tags `throwIfNothing` MissingRPMTag "Version"

    -- FIXME:  Where to get this from?
    source_ref = "SOURCE_REF"
 in
    Sources projectId license version source_ref
