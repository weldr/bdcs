module BDCS.Exceptions(DBException(..),
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
 deriving(Typeable)

instance Exception DBException

instance Show DBException where
    show (DBException s) = show s

throwIfNothing :: Exception e => Maybe a -> e -> a
throwIfNothing (Just v) _   = v
throwIfNothing _        exn = throw exn

throwIfNothingOtherwise :: Exception e => Maybe a -> e -> (a -> b) -> b
throwIfNothingOtherwise (Just v) _   fn = fn v
throwIfNothingOtherwise _        exn _  = throw exn
