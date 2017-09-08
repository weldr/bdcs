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
import Data.Maybe(mapMaybe)

import           BDCS.DB(Files(..), FileKeyValues(..))
import qualified BDCS.Label.Docs as Docs
import qualified BDCS.Label.InfoPage as Info
import qualified BDCS.Label.Library as Library
import qualified BDCS.Label.License as License
import qualified BDCS.Label.ManPage as Man
import qualified BDCS.Label.Service as Service
import           BDCS.Label.Types(Label(..))
import           BDCS.Label.Utils(addLabelKey)

import Utils.Monad(concatForM)

checks :: [(Files -> Bool, Files -> Maybe Label)]
checks = [(Docs.matches,    Docs.mkLabel),
          (Info.matches,    Info.mkLabel),
          (License.matches, License.mkLabel),
          (Library.matches, Library.mkLabel),
          (Man.matches,     Man.mkLabel),
          (Service.matches, Service.mkLabel)]

apply :: MonadIO m => [(Files, Key Files)] -> SqlPersistT m [Key FileKeyValues]
apply lst =
    -- Iterate over all the given files.
    concatForM lst $ \(f, ndx) -> do
        -- Gather up all the tuples from the checks list where the
        -- file met the matching criteria.
        let successfulChecks = filter (\(matches, _) -> matches f) checks

        -- Try to run the maker function from each of those tuples.
        -- It's possible for the maker function to return Nothing
        -- (though I don't know how that could happen right now),
        -- so we need to filter those out.
        let labels = mapMaybe (\(_, maker) -> maker f) successfulChecks

        -- Now add each of those labels to the database, collecting
        -- the resulting IDs.
        mapM (\lbl -> addLabelKey ndx lbl Nothing Nothing)
             labels
