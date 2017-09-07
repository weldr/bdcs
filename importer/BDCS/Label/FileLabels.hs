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

module BDCS.Label.FileLabels(apply)
 where

import Control.Monad.IO.Class(MonadIO)
import Database.Esqueleto(Key, SqlPersistT)

import           BDCS.DB(Files(..), FileKeyValues(..))
import qualified BDCS.Label.Docs as Docs
import qualified BDCS.Label.InfoPage as Info
import qualified BDCS.Label.Library as Library
import qualified BDCS.Label.ManPage as Man
import qualified BDCS.Label.Service as Service
import           BDCS.Label.Types(Label(..))
import           BDCS.Label.Utils(addLabelKey)

import Utils.Monad(concatForM)

checks :: [(Files -> Bool, Label)]
checks = [(Docs.matches,    DocsLabel),
          (Info.matches,    InfoPageLabel),
          (Library.matches, LibraryLabel),
          (Man.matches,     ManPageLabel),
          (Service.matches, ServiceLabel)]

apply :: MonadIO m => [(Files, Key Files)] -> SqlPersistT m [Key FileKeyValues]
apply lst =
    -- Iterate over the various file-related labels we know about.
    concatForM checks $ \(matches, ty) ->
        -- Iterate over all the given files.  If a file meets the matching criteria for
        -- this label, add a key.  Collect all the resulting IDs.
        mapM (\(_, ndx) -> addLabelKey ndx ty Nothing Nothing)
             (filter (\(f, _) -> matches f) lst)
