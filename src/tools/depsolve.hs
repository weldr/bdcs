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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Except(runExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import BDCS.DB(checkAndRunSqlite)
import BDCS.Depclose(depclose)
import BDCS.Depsolve(formulaToCNF, solveCNF)
import BDCS.Groups(groupIdToNevra)
import BDCS.Utils.Monad(mapMaybeM)

import Utils.GetOpt(commandLineArgs)

runCommand :: FilePath -> [String] -> IO ()
runCommand db things = do
    let things' = map T.pack things
    result <- runExceptT $ checkAndRunSqlite (T.pack db) $ do
        formula <- depclose ["x86_64"] things'
        solution <- solveCNF (formulaToCNF formula)

        -- solveCNF returns a list of (groupId, bool) assignments. Discard the False ones,
        -- and convert the True ids to nevras
        mapMaybeM groupIdToNevra $ map fst $ filter snd solution

    case result of
        Left e            -> print ("error: " ++ e) >> exitFailure
        -- Print the NEVRAs one per line
        Right assignments -> mapM_ TIO.putStrLn assignments

usage :: IO ()
usage = do
    putStrLn "Usage: depsolve metadata.db NEVRA [NEVRA ...]"
    exitFailure

main :: IO ()
main = commandLineArgs <$> getArgs >>= \case
    Just (db, _, args) -> runCommand db args
    _                  -> usage
