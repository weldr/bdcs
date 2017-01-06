-- Copyright (C) 2016 Red Hat, Inc.
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

import Conduit(($$), (=$), awaitForever, stdinC)
import Control.Monad(void)
import Control.Monad.Except(runExceptT)
import Control.Monad.IO.Class(MonadIO, liftIO)
import Data.Conduit(Consumer)
import Text.PrettyPrint(render)
import Text.PrettyPrint.HughesPJClass(Pretty(pPrint))

import RPM.Parse(parseRPMC)
import RPM.Types(RPM)

consumer :: (MonadIO m) => Consumer RPM m ()
consumer = awaitForever (liftIO . putStrLn . render . pPrint)

main :: IO ()
main =
    void $ runExceptT $ stdinC $$ parseRPMC =$ consumer
