{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: BDCS.ReqType
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Data types for working with the key in a 'BDCS.DB.KeyVal'.

module BDCS.KeyType(KeyType(..),
                    asText)
 where

import           Database.Persist.TH
import           Data.Aeson(ToJSON, toJSON)
import qualified Data.Text as T

import qualified BDCS.Label.Types as Label

-- | The 'BDCS.DB.KeyVal' record features a namespaced key, allowing multiple types of keys
-- to exist and be managed in the same database.  This type allows differentiation of the
-- multiple kinds of keys.
data KeyType = LabelKey Label.Label     -- ^ A key allowing only pre-defined labels to be appplied.
             | TextKey T.Text           -- ^ Ordinary text key
 deriving(Eq, Read, Show)

derivePersistField "KeyType"

instance ToJSON KeyType where
    toJSON (TextKey t)  = toJSON t
    toJSON (LabelKey l) = toJSON l

-- | Convert a 'KeyType' into 'Data.Text.Text'
asText :: KeyType -> T.Text
asText (LabelKey l) = T.pack $ show l
asText (TextKey t)  = t
