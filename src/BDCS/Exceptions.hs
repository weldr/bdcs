-- Copyright (C) 2016-2017 Red Hat, Inc.
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

module BDCS.Exceptions(DBException(..),
                       isDBExceptionException,
                       isMissingRPMTagException,
                       throwIfNothing,
                       throwIfNothingOtherwise)
 where

import Control.Exception(Exception, throw)
import Data.Data(Typeable)

-- A general purpose exception type for dealing with things that go wrong when working
-- with the database.  This could be broken out into a lot more type constructors to
-- make for an actually useful exception system.  In general, I dislike Haskell exceptions
-- but runSqlite will roll back the entire transaction if an exception is raised.  That's
-- a good reason to use them.
data DBException = DBException String
                 | MissingRPMTag String
 deriving(Eq, Typeable)

instance Exception DBException

instance Show DBException where
    show (DBException s)   = show s
    show (MissingRPMTag s) = "Missing required tag in RPM: " ++ s

throwIfNothing :: Exception e => Maybe a -> e -> a
throwIfNothing (Just v) _   = v
throwIfNothing _        exn = throw exn

throwIfNothingOtherwise :: Exception e => Maybe a -> e -> (a -> b) -> b
throwIfNothingOtherwise (Just v) _   fn = fn v
throwIfNothingOtherwise _        exn _  = throw exn

isDBExceptionException :: DBException -> Bool
isDBExceptionException (DBException _) = True
isDBExceptionException _               = False

isMissingRPMTagException :: DBException -> Bool
isMissingRPMTagException (MissingRPMTag _) = True
isMissingRPMTagException _                 = False
