-- |
-- Module: BDCS.Builds
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Utilities for working with database-related exceptions.

module BDCS.Exceptions(DBException(..),
                       isBadNameException,
                       isDBExceptionException,
                       isMissingRPMTagException,
                       throwIfNothing,
                       throwIfNothingOtherwise)
 where

import Control.Exception(Exception, throw)
import Data.Data(Typeable)

-- | A general purpose exception type for dealing with things that go wrong when working
-- with the database.  This type could grow into a more complex system in the future, if
-- needed.  This type is most helpful because runSqlite will roll back the entire
-- transaction if an exception is raised.
data DBException = DBException String           -- ^ A general purpose exception type,
                                                -- including an error message.
                 | MissingRPMTag String         -- ^ A required tag was missing from the
                                                -- RPM being processed.  The argument should
                                                -- be the name of the missing tag.
                 | BadName String               -- ^ The name of the package is not parseable.
 deriving(Eq, Typeable)

instance Exception DBException

instance Show DBException where
    show (BadName s)       = "Package name is not parseable: " ++ s
    show (DBException s)   = show s
    show (MissingRPMTag s) = "Missing required tag in RPM: " ++ s

-- | If a 'Maybe' value is Nothing, throw the given exception.  Otherwise, return the
-- value inside.
throwIfNothing :: Exception e => Maybe a -> e -> a
throwIfNothing (Just v) _   = v
throwIfNothing _        exn = throw exn

-- | If a 'Maybe' value is Nothing, throw the given exception.  Otherwise, run the
-- provided function on the value inside and return the result.
throwIfNothingOtherwise :: Exception e => Maybe a -> e -> (a -> b) -> b
throwIfNothingOtherwise (Just v) _   fn = fn v
throwIfNothingOtherwise _        exn _  = throw exn

-- | Is a given 'DBException' type a 'BadName'?
isBadNameException :: DBException -> Bool
isBadNameException (BadName _) = True
isBadNameException _           = False

-- | Is a given 'DBException' type the general 'DBException'?
isDBExceptionException :: DBException -> Bool
isDBExceptionException (DBException _) = True
isDBExceptionException _               = False

-- | Is a given 'DBException' type a 'MissingRPMTag'?
isMissingRPMTagException :: DBException -> Bool
isMissingRPMTagException (MissingRPMTag _) = True
isMissingRPMTagException _                 = False
