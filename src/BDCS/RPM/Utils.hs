{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.RPM.Utils
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Utility functions for "BDCS.RPM"

module BDCS.RPM.Utils(splitFilename)
 where

import           Data.Bifunctor(first)
import qualified Data.Text as T

-- | Turn an RPM filename in form of "NAME-[EPOCH:]VERSION-RELEASE.ARCH[.rpm]
-- into a tuple of (name, epoch, version, release, and arch).
splitFilename :: T.Text -> (T.Text, Maybe T.Text, T.Text, T.Text, T.Text)
splitFilename rpm_ = let
    rpm = if ".rpm" `T.isSuffixOf` rpm_ then T.dropEnd 4 rpm_ else rpm_

    (front,  a) = T.breakOnEnd "." rpm
    (front2, r) = T.breakOnEnd "-" $ T.dropWhileEnd (== '.') front
    (n,     ve) = first (T.dropWhileEnd (== '-')) $ T.breakOnEnd "-" $ T.dropWhileEnd (== '-') front2
    (e,      v) = first (T.dropWhileEnd (== ':')) $ T.breakOnEnd ":" ve
 in
    (n, if e == "" then Nothing else Just $ T.dropWhile (== ':') e, v, r, a)
