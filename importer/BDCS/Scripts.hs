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

{-# LANGUAGE RecordWildCards #-}

module BDCS.Scripts(findScript,
                    insertScript)
 where

import           Control.Monad.IO.Class(MonadIO)
import qualified Data.Text as T
import           Database.Esqueleto

import BDCS.DB

findScript :: MonadIO m => T.Text -> T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> SqlPersistT m (Maybe (Key Scripts))
findScript ty body name _ver _ndx _flags = firstResult $
    select $ from $ \script -> do
    where_ $ script ^. ScriptsTy ==. val ty &&.
             script ^. ScriptsBody ==. val body &&.
             (isNothing (val name) &&. isNothing (script ^. ScriptsTrigger_name) ||. script ^. ScriptsTrigger_name ==. val name)
    limit 1
    return $ script ^. ScriptsId

insertScript :: MonadIO m => Key Groups -> Scripts -> SqlPersistT m (Key GroupScripts)
insertScript groupId script@Scripts{..} =
    maybeKey (insert script >>= insert . GroupScripts groupId)
             (insert . GroupScripts groupId)
             (findScript scriptsTy scriptsBody scriptsTrigger_name scriptsTrigger_version scriptsTrigger_index scriptsTrigger_flags)
