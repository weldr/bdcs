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
import           Control.Monad.Except(runExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import BDCS.Depclose(depclose)
import BDCS.Depsolve(formulaToCNF, solveCNF)
import BDCS.Groups(groupIdToNevra)
import Utils.Monad(mapMaybeM)

usage :: IO ()
usage = do
    putStrLn "Usage: depsolve metadata.db NEVRA [NEVRA ...]"
    exitFailure

main :: IO ()
main = do
    argv <- getArgs

    when (length argv < 2) usage

    let db = T.pack $ head argv
    let things = map T.pack $ drop 1 argv
    result <- runExceptT $ runSqlite db $ do
        formula <- depclose ["x86_64"] things
        solution <- solveCNF (formulaToCNF formula)

        -- solveCNF returns a list of (groupId, bool) assignments. Discard the False ones,
        -- and convert the True ids to nevras
        mapMaybeM groupIdToNevra $ map fst $ filter snd solution

    case result of
        Left e            -> print ("error: " ++ e) >> exitFailure
        -- Print the NEVRAs one per line
        Right assignments -> mapM_ TIO.putStrLn assignments
