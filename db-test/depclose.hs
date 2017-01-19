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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- This program takes a database and an RPM name (or a list of RPMs) and returns
-- a list of all the dependencies of that RPM.  If multiple RPMs are provided, it
-- handles each one separately - not as several packages as part of a whole.
--
-- This program does not attempt to do dependency solving.  It simply gathers up
-- everything that a package says it depends on, and everything that all those
-- say that depend on, and so forth.  If there's a problem (dependency is not
-- present in the database, versions are incorrect, etc.) this program will not
-- error out.  It is up to the caller to do the solving and figure problems
-- out.
--
-- If a dependency can be satisfied in multiple ways, this program should grab
-- all possibilities and all their dependencies.  In general, any decisions about
-- the best way to solve something is handled by the caller.
--
-- Limitations:
--
-- * Should handle multiple RPMs as part of the same "transaction".
-- * Doesn't know why a dependency got pulled in, but maybe that should be left up
--   to the caller as well.
-- * Attempts to handle versions on Requires: but perhaps doesn't do a good job
--   of that.  This needs more thorough testing.
-- * Should handle all the other kinds of dependencies, after we start putting
--   those into the database.
-- * Has not been tested against more complicated cases (packages that can have a
--   dep satisfied two different ways, lang packs, multilib, etc.).
-- * Has not been shown to give the same results as yum/dnf.

import           Control.Monad(liftM, when)
import           Control.Monad.IO.Class(MonadIO)
import           Data.List(isInfixOf)
import           Data.Maybe(catMaybes, fromMaybe, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import BDCS.DB

data NEVRA = NEVRA String String String String String
 deriving(Eq, Ord, Show)

printNEVRA :: NEVRA -> String
printNEVRA (NEVRA n "" v r a)   =             n ++ "-" ++ v ++ "-" ++ r ++ "." ++ a
printNEVRA (NEVRA n e v r a)    = e ++ ":" ++ n ++ "-" ++ v ++ "-" ++ r ++ "." ++ a

-- Look up a key/value pair for the group with the given name.  It is assumed there
-- will only be one key/value pair.
getValueForGroupName :: MonadIO m => String -> String -> SqlPersistT m (Maybe String)
getValueForGroupName name key = do
    ndx <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` group) -> do
           on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
                    group_keyval ^. GroupKeyValuesGroup_id ==. group ^. GroupsId
           where_ $ group ^. GroupsName ==. val name &&.
                    keyval ^. KeyValKey_value ==. val key
           limit 1
           return $ keyval ^. KeyValVal_value
    return $ listToMaybe (map unValue ndx)

-- Given a group name, return the complete NEVRA tuple for it.  Each element
-- except for the name can potentially be empty.
getNEVRAForGroupName :: MonadIO m => String -> SqlPersistT m NEVRA
getNEVRAForGroupName name = do
    epoch   <- fromMaybe "" <$> getValueForGroupName name "epoch"
    ver     <- fromMaybe "" <$> getValueForGroupName name "version"
    release <- fromMaybe "" <$> getValueForGroupName name "release"
    arch    <- fromMaybe "" <$> getValueForGroupName name "arch"
    return $ NEVRA name epoch ver release arch

-- Given a group name, return its index in the groups table.
findGroupId :: MonadIO m => String -> SqlPersistT m (Maybe GroupsId)
findGroupId name = do
    ndx <- select $ from $ \groups -> do
           where_ $ groups ^. GroupsName ==. val name
           limit 1
           return $ groups ^. GroupsId
    return $ listToMaybe (map unValue ndx)

-- Given a group's index into the groups table, return its name.
findGroupName :: MonadIO m => GroupsId -> SqlPersistT m (Maybe String)
findGroupName ndx = do
    row <- select $ from $ \groups -> do
           where_ $ groups ^. GroupsId ==. val ndx
           limit 1
           return $ groups ^. GroupsName
    return $ listToMaybe (map unValue row)

-- Given the name of something that is required by a package, look up everything
-- that provides it and return their indices.  It's possible for multiple things
-- to satisfy the same requirement.
findProviderId :: MonadIO m => String -> SqlPersistT m [GroupsId]
findProviderId thing = do
    ndx <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval) -> do
           on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id
           -- A requirement could be satisfied by either exactly matching what was
           -- asked for (which could be an expression), or by matching the package
           -- name.  Package names are not listed as an rpm-provide typically.
           -- FIXME:  Might need to change this for expressions.  We can't check
           -- versions here but maybe the right thing to do is just strip out
           -- versioning from "thing" and grab everything that starts with it.
           where_ $ keyval ^. KeyValVal_value ==. val thing ||.
                    keyval ^. KeyValKey_value ==. val "packageName" &&. keyval ^. KeyValVal_value ==. val thing
           return $ group_keyval ^. GroupKeyValuesGroup_id
    return $ map unValue ndx

-- Given a file path, look up everything that provides it and return their indices.
-- It's possible for multiple things to provide the same file.
findGroupContainingFile :: MonadIO m => String -> SqlPersistT m [GroupsId]
findGroupContainingFile file = do
    ndx <- select $ distinct $ from $ \(group `InnerJoin` files) -> do
           on     $ group ^. GroupFilesFile_id ==. files ^. FilesId
           where_ $ files ^. FilesPath ==. val file
           return $ group ^. GroupFilesGroup_id
    return $ map unValue ndx

-- Return the group name for everything that provides the given name.  This uses
-- findProviderId inside, but that just returns database indices.  Those need to
-- be converted into human-readable names.  This function will probably need to
-- be changed in the future to return some more complicated structure so a caller
-- can make decisions.  Just the name won't be enough.
whatProvides :: MonadIO m => String -> SqlPersistT m [String]
whatProvides name = do
    providerIds <- findProviderId name
    catMaybes <$> mapM findGroupName providerIds

-- Return the group name for everything that provides the given file path.  We store
-- files and requires/provides separately, so this requires two lookup functions.
-- Just like whatProvides, this is a wrapper to convert database indices into names.
-- And also just like whatProvides, its return value will need to get more complex
-- in the future.
whatProvidesFile :: MonadIO m => String -> SqlPersistT m [String]
whatProvidesFile name = do
    providerIds <- findGroupContainingFile name
    catMaybes <$> mapM findGroupName providerIds

-- Given a group name, look up everything it requires and return those expressions.
-- The requires expressions can either just be a name ("Requires: blah") or a name
-- plus version information ("Requires: blah > 2").  Nothing else is supported at
-- this time.
getRequirements :: MonadIO m => String -> SqlPersistT m [String]
getRequirements name = findGroupId name >>= \case
    Nothing  -> return []
    Just ndx -> do
        reqs <- select $ distinct $ from $ \(req `InnerJoin` group_reqs) -> do
                on     $ req ^. RequirementsId ==. group_reqs ^. GroupRequirementsReq_id
                -- Filter out rpmlib() requirements.  Those don't appear to be provided
                -- by anything so they will just always be unsolvable.  I don't think
                -- we care about them anyway.  There are other types of these requires,
                -- too (config() comes to mind) but those appear to be provided by
                -- something.  So they can stay in the results for now.
                where_ $ group_reqs ^. GroupRequirementsGroup_id ==. val ndx &&.
                         not_ (req ^. RequirementsReq_expr `like` val "rpmlib" ++. (%))
                return $ req ^. RequirementsReq_expr
        return $ map unValue reqs

-- And then this is the worklist function that actually gathers up the dependency tree
-- for a given package and returns it as a list of NEVRAs.
closeRPM :: FilePath -> String -> IO [NEVRA]
closeRPM db rpm = runSqlite (T.pack db) $ do
    -- Pre-seed the list with the top-level requirements of the rpm.  The sets of
    -- things we've determined are deps and the things we've already seen are
    -- initialized to empty.
    --
    -- This is two different sets so we know what's a package providing a dependency
    -- (stored in the first set) and what's an abstract dependency that we have already
    -- seen and potentially gathered (stored in the second set, things like "libc.so.6"
    -- or "config(whatever)").  We do not want to return the latter as results.
    toplevel <- getRequirements rpm
    doit toplevel Set.empty Set.empty
 where
    doit []      deps _    = return $ Set.toList deps
    doit (hd:tl) deps seen =
        -- Seen it before, don't gather it up again.
        if | hd `Set.member` seen   -> doit tl deps seen
        -- This is a file requirement.  Get everything that provides it, add those to
        -- the dependencies set, mark as seen, and call this function again.  We add
        -- the providers to the worklist so we can gather all their dependencies, too.
           | "/" `isInfixOf` hd     -> do providers <- whatProvidesFile hd
                                          nevras <- mapM getNEVRAForGroupName providers
                                          let deps' = deps `Set.union` Set.fromList nevras
                                          let seen' = Set.insert hd seen
                                          doit (providers ++ tl) deps' seen'
        -- This is either a package dependency or a dependency on something more abstract
        -- like an soname or feature (config(), etc.).  Get everything that provides it,
        -- add those to the dependencies set, mark as seen, and call this function again.
        -- This is the only place we call getRequirements, too.  Add all the top-level
        -- requirements of all of this thing's providers to the worklist so they can be
        -- further gathered up.  Calling it here should take care of anything added as a
        -- result of file dependencies.  (This assumption will fail if files can somehow
        -- depend on other files.)
           | otherwise              -> do providers <- whatProvides hd
                                          r <- liftM concat (mapM getRequirements providers)
                                          nevras <- mapM getNEVRAForGroupName providers
                                          let deps' = deps `Set.union` Set.fromList nevras
                                          let seen' = Set.insert hd seen
                                          doit (r ++ providers ++ tl) deps' seen'

printResult :: String -> [NEVRA] -> IO ()
printResult rpm deps = do
    putStrLn $ "*** Dependencies for " ++ rpm ++ ":"
    mapM_ (\dep -> putStrLn $ "\t" ++ printNEVRA dep) deps

main :: IO ()
main = do
    -- Read the list of RPMs to find all the dependencies of.
    argv <- getArgs

    when (length argv < 2) $ do
        putStrLn "Usage: depclose metadata.db RPM [RPM ...]"
        exitFailure

    let db   = head argv
    let rpms = tail argv

    mapM_ (processOne db) rpms
 where
    processOne db rpm = do
        result <- closeRPM db rpm
        printResult rpm result
