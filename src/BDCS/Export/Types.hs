-- |
-- Module: BDCS.Export.Types
-- Copyright: (c) 2018 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Types related to exporting.

module BDCS.Export.Types(ExportType(..),
                         exportTypeText,
                         exportTypeFromText,
                         supportedExportTypes)
 where

import qualified Data.Text as T

data ExportType = ExportDirectory
                | ExportOstree
                | ExportQcow2
                | ExportTar
 deriving(Eq, Show)

exportTypeText :: ExportType -> T.Text
exportTypeText ExportDirectory = "directory"
exportTypeText ExportOstree    = "ostree"
exportTypeText ExportQcow2     = "qcow2"
exportTypeText ExportTar       = "tar"

exportTypeFromText :: T.Text -> Maybe ExportType
exportTypeFromText t = case T.toLower (T.strip t) of
    "directory" -> Just ExportDirectory
    "ostree"    -> Just ExportOstree
    "qcow2"     -> Just ExportQcow2
    "tar"       -> Just ExportTar
    _           -> Nothing

supportedExportTypes :: [ExportType]
supportedExportTypes = [ExportDirectory, ExportOstree, ExportQcow2, ExportTar]
