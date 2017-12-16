-- |
-- Module: BDCS.Utils.Filesystem
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Filesystem related utility functions

module BDCS.Utils.Filesystem(doesPathExist)
 where

import Control.Exception(catchJust)
import Control.Monad(guard)
import System.IO.Error(doesNotExistErrorType, ioeGetErrorType)
import System.Posix.Files(getSymbolicLinkStatus)

-- | A version of doesPathExist that also returns True if the path exists
-- and is a broken symlink.
doesPathExist :: FilePath -> IO Bool
doesPathExist path = catchJust isENOENT getStatus (const $ return False)
 where
    getStatus = getSymbolicLinkStatus path >> return True

    isENOENT :: IOError -> Maybe ()
    isENOENT e = guard $ ioeGetErrorType e == doesNotExistErrorType
