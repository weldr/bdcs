-- Copyright (C) 2017 Red Hat, Inc.
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
{-# LANGUAGE TemplateHaskell #-}

module BDCS.Label.Types(Label(..),
                        labelDescriptions)
 where

import           Data.Aeson((.=), ToJSON, object, toJSON)
import qualified Data.Text as T
import           Database.Persist.TH

data Label = DocsLabel
           | FontsLabel
           | InfoPageLabel
           | LibraryLabel
           | LicenseLabel
           | ManPageLabel
           | ServiceLabel
           | TranslationLabel T.Text
    deriving(Eq, Read, Show)

instance ToJSON Label where
    toJSON (TranslationLabel lang) = object ["TranslationLabel" .= toJSON lang]
    toJSON lbl                     = toJSON $ T.pack $ show lbl

derivePersistField "Label"

labelDescriptions :: [(String, String)]
labelDescriptions = [
    ("DocsLabel",        "Documentation - mostly things in /usr/share/doc"),
    ("FontsLabel",       "Fonts"),
    ("InfoPageLabel",    "GNU info pages"),
    ("LibraryLabel",     "Static and shared libraries"),
    ("LicenseLabel",     "Files containing a software license"),
    ("ManPageLabel",     "man pages"),
    ("ServiceLabel",     "systemd .service files"),
    ("TranslationLabel", "Translations - takes a language code as argument")
 ]
