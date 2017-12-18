{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: BDCS.Export.Tar
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Functions for exporting objects from the BDCS into a tar file.

module BDCS.Export.Tar(tarSink)
 where

import qualified Codec.Archive.Tar as Tar
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.ByteString.Lazy(writeFile)
import           Data.Conduit(Consumer)
import qualified Data.Conduit.List as CL
import           Prelude hiding(writeFile)

-- | A 'Consumer' that writes objects (in the form of 'Tar.Entry' records) into a tar archive
-- with the provided name.  To convert objects into an Entry, see 'BDCS.CS.objectToTarEntry'.
tarSink :: MonadIO m => FilePath -> Consumer Tar.Entry m ()
tarSink out_path = do
    entries <- CL.consume
    liftIO $ writeFile out_path (Tar.write entries)
