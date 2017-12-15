{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: BDCS.Version
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Extract the version number of bdcs tools.

module BDCS.Version(printVersion)
  where

import Data.Version (showVersion)
import Development.GitRev
import Text.Printf(printf)

import Paths_bdcs (version)

-- | Given the name of a bdcs tool, print its version number.
printVersion :: String -> IO ()
printVersion toolName = do
    let git_version = $(gitDescribe)
    if git_version == "UNKNOWN" then
        printf "%s v%s\n" toolName $ showVersion version
    else
        printf "%s %s\n" toolName git_version
