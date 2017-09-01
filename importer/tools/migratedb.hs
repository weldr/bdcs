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

import           Control.Monad(when)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.Text as T
import           Database.Persist.Sql(SqlPersistT, rawExecute)
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import BDCS.DB(getDbVersion)

-- new column groups.build_id. Existing groups will have no corresponding builds,
-- so just add the column and let it be null.
-- new table source_files and associated indexes, starts off empty.
migrateV1 :: MonadIO m => SqlPersistT m ()
migrateV1 = do
    liftIO $ putStrLn "Migrating from version 1 to version 2..."
    rawExecute "ALTER TABLE groups ADD COLUMN build_id integer references builds(id) null" []
    rawExecute (T.concat ["CREATE TABLE source_files (",
                              "id integer primary key,",
                              "source_id integer references sources(id) not null,",
                              "file_id integer references files(id) not null",
                          ")"]) []
    rawExecute "CREATE INDEX source_files_source_id_idx on source_files(source_id)" []
    rawExecute "CREATE INDEX source_files_file_id_idx on source_files(file_id)" []
    rawExecute "PRAGMA user_version = 2" []

usage :: IO ()
usage = do
    putStrLn "Usage: migratedb metadata.db"
    exitFailure

main :: IO ()
main = do
    argv <- getArgs

    when (length argv /= 1) usage

    runSqlite (T.pack $ head argv) $ do
        userVersion <- getDbVersion

        liftIO $ putStrLn $ "Current user_version: " ++ show userVersion

        -- too old, don't know how to upgrade
        when (userVersion < 1) $ liftIO (putStrLn "Unable to upgrade database, user_version is too old" >> exitFailure)

        when (userVersion < 2) migrateV1

        liftIO $ putStrLn "done."
