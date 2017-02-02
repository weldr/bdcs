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

{-# LANGUAGE CPP #-}
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

import           Control.Conditional(ifM)
import           Control.Monad(forM, forM_, liftM, when)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.List(isInfixOf)
import           Data.Maybe(catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)

import BDCS.DB

data Proposition = String `Obsoletes` String
                 | NEVRA  `Provides`  String
                 | NEVRA  `Requires`  String
 deriving(Eq, Ord, Show)

data NEVRA = NEVRA String String String String String
 deriving(Eq, Ord, Show)

printNEVRA :: NEVRA -> String
printNEVRA (NEVRA n "" v r a)   =             n ++ "-" ++ v ++ "-" ++ r ++ "." ++ a
printNEVRA (NEVRA n e v r a)    = e ++ ":" ++ n ++ "-" ++ v ++ "-" ++ r ++ "." ++ a

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM fn lst = liftM concat (mapM fn lst)

-- Look up a key/value pair for the group with the given GroupsId.  It is assumed there
-- will only be one key/value pair.
getValueForGroup :: MonadIO m => GroupsId -> String -> SqlPersistT m (Maybe String)
getValueForGroup grp key = do
    ndx <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` group) -> do
           on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
                    group_keyval ^. GroupKeyValuesGroup_id ==. group ^. GroupsId
           where_ $ group ^. GroupsId ==. val grp &&.
                    keyval ^. KeyValKey_value ==. val key
           limit 1
           return $ keyval ^. KeyValVal_value
    return $ listToMaybe (map unValue ndx)

-- Given a group id, return the complete NEVRA tuple for it.  Each element
-- except for the name can potentially be empty.
getNEVRAForGroupId :: MonadIO m => GroupsId -> SqlPersistT m NEVRA
getNEVRAForGroupId id = do
    name   <- fromMaybe "" <$> getValueForGroup id "name"
    epoch   <- fromMaybe "" <$> getValueForGroup id "epoch"
    ver     <- fromMaybe "" <$> getValueForGroup id "version"
    release <- fromMaybe "" <$> getValueForGroup id "release"
    arch    <- fromMaybe "" <$> getValueForGroup id "arch"
    return $ NEVRA name epoch ver release arch

-- Given a group name, return its index in the groups table.
findGroupId :: MonadIO m => String -> SqlPersistT m [GroupsId]
findGroupId name = do
    ndx <- select $ from $ \groups -> do
           where_ $ groups ^. GroupsName ==. val name
           return $ groups ^. GroupsId
    return $ map unValue ndx

-- Given the name of something that is required by a package, look up everything
-- that provides it and return their indices and the expression provided.  It's possible
-- for multiple things to satisfy the same requirement.
findProviderForName :: MonadIO m => String -> SqlPersistT m [(GroupsId, Proposition)]
findProviderForName thing = do
    let baseThing = takeWhile (/= ' ') thing
    ndx <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval) -> do
           on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id
           -- A requirement is satisfied by matching a rpm-provide, which may be
           -- an exact match (name-only) or a versioned match (name = version).
           -- For provides with versions, ignoring the version and grabbing everything.
           where_ (keyval ^. KeyValKey_value ==. val "rpm-provide" &&.
                   keyval ^. KeyValVal_value ==. val baseThing)
           return (group_keyval ^. GroupKeyValuesGroup_id, keyval ^. KeyValExt_value)

    -- unpack the database values into something useful
    let (ids, provides) = unzip ndx
    let ids' = map unValue ids
    let provides' = map (fromMaybe "" . unValue) provides

    -- get the NEVRA for each group id returned, generate a propsition
    nevras <- mapM getNEVRAForGroupId ids'
    let propositions = zipWith Provides nevras provides'

    -- return each group id with the associated proposition
    return $ zip ids' propositions

-- Given a file path, look up everything that provides it and return their indices.
-- and the expression provided. It's possible for multiple things to provide the same file.
findGroupContainingFile :: MonadIO m => String -> SqlPersistT m [(GroupsId, Proposition)]
findGroupContainingFile file = do
    ndx <- select $ distinct $ from $ \(group `InnerJoin` files) -> do
           on     $ group ^. GroupFilesFile_id ==. files ^. FilesId
           where_ $ files ^. FilesPath ==. val file
           return (group ^. GroupFilesGroup_id, files ^. FilesPath)

    let (ids, paths) = unzip ndx
    let ids' = map unValue ids
    let paths' = map unValue paths

    nevras <- mapM getNEVRAForGroupId ids'
    let propositions = zipWith Provides nevras paths'
    return $ zip ids' propositions

-- Given a group id, return everything that obsoletes that group.  This is returned
-- as both the name of the thing and the related expression.  We may need this expression
-- for checking version numbers later.
whatObsoletes :: MonadIO m => GroupsId -> SqlPersistT m [(String, String)]
whatObsoletes id = do
    ndx <- select $ distinct $ from $ \(keyval `InnerJoin` group_keyval `InnerJoin` group) -> do
           on     $ keyval ^. KeyValId ==. group_keyval ^. GroupKeyValuesKey_val_id &&.
                    group_keyval ^.GroupKeyValuesGroup_id ==. group ^. GroupsId
           where_ $ group ^. GroupsId ==. val id &&.
                    keyval ^. KeyValKey_value ==. val "rpm-obsolete"
           return  (group ^. GroupsName, keyval ^. KeyValExt_value)
    return $ mapMaybe (\(a, b) -> case unValue b of
                                      Nothing -> Nothing
                                      Just b' -> Just (unValue a, b')) ndx

-- Given a group id, look up everything it requires and return those expressions.
-- The requires expressions can either just be a name ("Requires: blah") or a name
-- plus version information ("Requires: blah > 2").  Nothing else is supported at
-- this time.
getRequirements :: MonadIO m => GroupsId -> SqlPersistT m [String]
getRequirements id = do
    reqs <- select $ from $ \(req `InnerJoin` group_reqs) -> do
            on     $ req ^. RequirementsId ==. group_reqs ^. GroupRequirementsReq_id
            -- Filter out rpmlib() requirements.  Those don't appear to be provided
            -- by anything so they will just always be unsolvable.  I don't think
            -- we care about them anyway.  There are other types of these requires,
            -- too (config() comes to mind) but those appear to be provided by
            -- something.  So they can stay in the results for now.
            where_ $ group_reqs ^. GroupRequirementsGroup_id ==. val id &&.
                     not_ (req ^. RequirementsReq_expr `like` val "rpmlib" ++. (%))
            return $ req ^. RequirementsReq_expr
    return $ map unValue reqs

-- And then this is the worklist function that actually gathers up the dependency tree
-- for a list of packages and returns it as a list of NEVRAs.  It's easy to do several
-- RPMs at once if they are all given to this function at the same time.  It prevents
-- having to save the working data.
closeRPM :: FilePath -> [String] -> IO [Proposition]
closeRPM db rpms = runSqlite (T.pack db) $
    -- The sets of things we've determined are deps and the things we've already seen
    -- are initialized to empty.
    --
    -- This is two different sets so we know what's a package providing a dependency
    -- (stored in the first set) and what's an abstract dependency that we have already
    -- seen and potentially gathered (stored in the second set, things like "libc.so.6"
    -- or "config(whatever)").  We do not want to return the latter as results.
    doit rpms Set.empty Set.empty
 where
    doit []      props _    = return $ Set.toList props
    -- Seen it before, don't gather it up again.
    doit (hd:tl) props seen | hd `Set.member` seen = doit tl props seen
    -- This is either a package dependency or a dependency on something more abstract
    -- like an soname or feature (config(), etc.).  Get everything that provides it,
    -- add those to the dependencies set, mark as seen, and call this function again.
    -- This is the only place we call getRequirements, too.  Add all the top-level
    -- requirements of all of this thing's providers to the worklist so they can be
    -- further gathered up.
    -- Dependencies that look like paths could be a path, or could match an abstract
    -- feature, so they get checked twice.
    doit (hd:tl) props seen | otherwise = do
        providers <- findProviderForName hd
#if DEBUG
        liftIO $ do
            putStrLn $ "Providers for " ++ hd ++ " are:"
            mapM_ (putStrLn . show . snd) providers
            putStrLn "Gathering requirements"
#endif
        -- If the requirment looks like a filename, also look for packages
        -- providing the file
        fileProviders <- ifM (return $ head hd == '/')
                             (findGroupContainingFile hd)
                             (return [])
        let providers' = providers ++ fileProviders

        reqs <- mapM (getRequirements . fst) providers'
#if DEBUG
        liftIO $ do
            putStrLn "Requirements are:"
            forM_ (zip providers' reqs) $ \((_, thing), rs) -> do
                putStrLn $ "Requirements for " ++ (show thing) ++ ":"
                mapM_ putStrLn rs
            putStrLn "Gathering group names"
#endif
        nevras <- mapM (getNEVRAForGroupId . fst) providers'
#if DEBUG
        liftIO $ do
            putStrLn "Groups are:"
            mapM_ (putStrLn . printNEVRA) nevras
            putStrLn "Gathering obsoletes"
#endif
        obsoletes <- mapM (whatObsoletes . fst) providers'
#if DEBUG
        liftIO $ do
            putStrLn "Obsoletes are:"
            forM_ (zip providers' obsoletes) $ (\thing, obs) -> do
                putStrLn $ "Obsoletes " ++ thing ++ ":"
                mapM_ putStrLn obs
#endif
        let props' = props `Set.union` Set.fromList (map snd providers')
                           `Set.union` Set.fromList (concatMap (\(t, rs) -> map (t `Requires`) rs)
                                                               (zip nevras reqs))
                           `Set.union` Set.fromList (map (uncurry Obsoletes) (concat obsoletes))
        let seen' = Set.insert hd seen
        doit (concat reqs ++ map fst (concat obsoletes) ++ tl) props' seen'

printResult :: [Proposition] -> IO ()
printResult props =
    mapM_ (putStrLn . printOne) props
 where
    printOne (left  `Obsoletes` right) = left ++ " obsoletes " ++ right
    printOne (nevra `Provides`  thing) = printNEVRA nevra ++ " provides " ++ thing
    printOne (nevra `Requires`  thing) = printNEVRA nevra ++ " requires " ++ thing

main :: IO ()
main = do
    -- Read the list of RPMs to find all the dependencies of.
    argv <- getArgs

    when (length argv < 2) $ do
        putStrLn "Usage: depclose metadata.db RPM [RPM ...]"
        exitFailure

    let db   = head argv
    let rpms = tail argv

    result <- closeRPM db rpms
    printResult result
