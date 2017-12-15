{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: BDCS.Depclose
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Collect all the dependencies for a package, but do not solve them.

module BDCS.Depclose(DepFormula,
                     depclose)
 where

import           Codec.RPM.Version(DepRequirement(..), EVR(..), parseDepRequirement, satisfies)
import qualified Codec.RPM.Version as RPM(DepOrdering(EQ))
import           Control.Monad(filterM, foldM, when)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Bifunctor(first)
import           Data.List(intersect)
import           Data.Maybe(fromMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Database.Persist.Sql(SqlPersistT)

import           BDCS.Depsolve(Formula(..))
import           BDCS.DB
import           BDCS.Files(pathToGroupId)
import           BDCS.Groups(getGroupId, getRequirementsForGroup)
import           BDCS.GroupKeyValue(getGroupsByKeyVal, getKeyValuesForGroup, getValueForGroup)
import           BDCS.KeyType
import qualified BDCS.ReqType as RT
import           BDCS.Utils.Error(errorToMaybe)
import           BDCS.Utils.Monad(concatMapM, foldMaybeM, mapMaybeM)

data ParentItem = GroupId (Key Groups)
                | Provides DepRequirement
 deriving (Eq, Ord)

-- The Set is used to store the groups that are parents of the current subexpression,
-- used to detect dependency loops and stop recursion. For instance:
--
--  A Requires B
--  B Requires C
--  C Requires A
--
-- When depclose gets to C Requires A it can stop, since that has already been resolved.
type DepParents = Set.Set ParentItem

-- | Type of the depclose results - see 'BDCS.Depsolve.Formula'
type DepFormula = Formula (Key Groups)

-- | Given a path to a mddb, a list of architectures, and a list of RPMS, return a formula describing the dependencies
-- The general idea is, given a list of packages to depclose, convert each to a group id, and for each id:
--    - gather the conflict and obsolete information, find matching group ids, express as Not conflict/obsolete-id
--    - gather the requirement expressions, for each:
--       * find a list of matching group ids
--       * if empty, the dependency is not satisfiable
--       * recurse on each group id to gather the requirements of the requirement
--       * return the expression as an Or of the matching group ids
--    - return the whole thing as an And [self, conflict/obsolete information, requirement information]
--
-- Everything is run in a state with two components: a Map from groupid to expression to act as a cache,
-- and a Set containing the group ids that are part of the current branch of the dependency tree in order
-- to detect and ignore loops.
depclose :: (MonadError String m, MonadIO m) => [T.Text] -> [T.Text] -> SqlPersistT m DepFormula
depclose arches nevras = do
    -- Convert each NEVRA into a group ID.
    groupIds <- mapM getGroupId nevras

    -- resolve each group id into a DepFormula
    -- Use foldM to pass the parents set from resolving one group into the next group, so we
    -- don't depclose things already depclosed from a previous group ID.
    (formulas, _) <- foldM foldIdToFormula ([], Set.empty) groupIds

    -- Every requirement in the list is required, so the final result is an And of the individual results.
    return $ And formulas
 where
    -- turn groupIdToFormula into something we can use with fold
    foldIdToFormula :: (MonadError String m, MonadIO m) => ([DepFormula], DepParents) -> Key Groups -> SqlPersistT m ([DepFormula], DepParents)
    foldIdToFormula (formulaAcc, parents) groupId = first (:formulaAcc) <$> groupIdToFormula parents groupId

    -- convert a group id to a dependency formula. First, check the cache to see if we've already gathered this group id
    groupIdToFormula :: (MonadError String m, MonadIO m) => DepParents -> Key Groups -> SqlPersistT m (DepFormula, DepParents)
    groupIdToFormula parents groupId = do
        -- add this group id to the parents set
        let parents' = Set.insert (GroupId groupId) parents

        -- grab the key/value based data
        conflicts <- getKeyValuesForGroup groupId (Just $ TextKey "rpm-conflict") >>= mapM kvToDep
        obsoletes <- getKeyValuesForGroup groupId (Just $ TextKey "rpm-obsolete") >>= mapM kvToDep

        -- map the strings to group ids. Obsolete and Conflict both express a potential group id
        -- that should NOT be included in the final, depsolved result, so express that information here
        -- as Not <matching-ids>
        --
        -- In RPM headers: the expressions in Conflicts headers match corresponding names in the Provides
        -- headers. Obsoletes, however, matches package names.
        conflictIds <- concatMapM providerIds conflicts
        obsoleteIds <- concatMapM nameReqIds obsoletes
        let obsConflictFormulas = map Not (conflictIds ++ obsoleteIds)

        -- grab all of the providers strings and add them to the parents set
        -- Saving this data allows us to avoid repeatedly depclosing over a requirement provided
        -- by more than one group. For instance, if there's more than one version of glibc available
        -- in the mddb, a requirement for "libc.so.6" might resolve to Or [glibc-1, glibc-2]. Since
        -- there are two choices, we can't say that either group id is definitely part of the expression,
        -- but "libc.so.6" is definitely solved as part of the expression and does not need to be repeated.
        providesSet <- Set.union parents'
                    <$> Set.fromList
                    <$> map Provides
                    <$> (getKeyValuesForGroup groupId (Just $ TextKey "rpm-provide") >>= mapM kvToDep)

        -- Now the recursive part. First, grab everything from the requirements table for this group:
        -- TODO maybe do something with strength, context, etc.

        requirements <- getRequirementsForGroup groupId RT.Runtime >>= mapM reqToDep

        -- Resolve each requirement to a list of group ids. Each group id is a possibility for satisfying
        -- the requirement. An empty list means the requirement cannot be satisfied.
        -- Zip the list of ids with the original requirement for error reporting.
        requirementIds <- zip requirements <$> mapM providerIds requirements

        -- Resolve each list of group ids to a formula
        -- Fold the parents set returned by each requirement into the next requirement, so we don't repeat
        -- ourselves too much.
        (requirementFormulas, requirementParents) <- foldMaybeM resolveOneReq ([], providesSet) requirementIds

        -- add an atom for the groupId itself, And it all together
        return (And (Atom groupId : obsConflictFormulas ++ requirementFormulas), requirementParents)
     where
        resolveOneReq :: (MonadError String m, MonadIO m) => ([DepFormula], DepParents) -> (DepRequirement, [Key Groups]) -> SqlPersistT m (Maybe ([DepFormula], DepParents))
        resolveOneReq (formulaAcc, parentAcc) (req, idlist) =
               -- If any of the possible ids are in the parents set, the requirement is already satisfied in the parents
            if | any (`Set.member` parentAcc) (map GroupId idlist) -> return Nothing
               -- If this exact requirement is already in the parents set, the requirement is already solved, so skip this group
               | Set.member (Provides req) parentAcc -> return Nothing
               | otherwise -> do
                -- map each possible ID to a forumula, discarding the ones that cannot be satisfied
                (formulaList, parentList) <- unzip <$> mapMaybeM (errorToMaybe . groupIdToFormula parentAcc) idlist

                -- If nothing worked, that's an error
                when (null formulaList) $ throwError $ "Unable to resolve requirement: " ++ show req

                -- The solution to this requirement is an Or of all the possibilities
                -- The group ids that are definitely required by this formulas is the intersection of all of the individual sets
                let reqFormula = Or formulaList
                let reqParents = foldl1 Set.intersection parentList

                -- Add the results to the accumulators
                return $ Just (reqFormula : formulaAcc, Set.union parentAcc reqParents)

    -- convert requirements to group IDs

    -- Given a DepRequirement, return the group ids with a matching rpm-provide key/val
    providerIds :: (MonadError String m, MonadIO m) => DepRequirement -> SqlPersistT m [Key Groups]
    providerIds req = do
        -- Pull the name out of the requirement
        let DepRequirement reqname _ = req

        -- Find all groups with a matching rpm-provide
        vals <- getGroupsByKeyVal "rpm" (TextKey "rpm-provide") (Just reqname)

        -- Filter out any that don't have a matching version
        -- convert the second part of the tuple (the KeyVal) to a Dep and check it against the input req
        valsVersion <- filterM (fmap (`satisfies` req) . kvToDep . snd) vals

        -- we're done with the actual expression now, just need the group ids
        let valsVersionIds = map fst valsVersion

        -- Filter out the ones with the wrong arch
        providerVals <- filterM matchesArch valsVersionIds

        -- If the requirement looks like a filename, check for groups providing the file *in addition to* rpm-provide
        fileVals <- if "/" `T.isPrefixOf` reqname then pathToGroupId reqname >>= filterM matchesArch
                                                  else return []

        return $ providerVals ++ fileVals

    -- Given a DepRequirement, return the group ids that match by name.
    -- This is used to satisfy Obsoletes
    nameReqIds :: MonadIO m => DepRequirement -> SqlPersistT m [Key Groups]
    nameReqIds req = do
        -- Pull the name out of the requirement
        let DepRequirement reqname _ = req

        vals <- map fst <$> getGroupsByKeyVal "rpm" (TextKey "name") (Just reqname)

        -- filter out the values that don't match by arch
        valsArch <- filterM matchesArch vals

        -- If there is no version in the DepRequirement we're trying to satisfy, we're done.
        -- otherwise, grab more info from the mddb to turn each group id into a name = EVR DepRequirement,
        -- and filter out the ones that do not satisfy the version.
        case req of
            DepRequirement _ Nothing  -> return valsArch
            DepRequirement _ (Just _) -> filterM (\gid -> do
                                                    providerReq <- groupIdToDep reqname gid
                                                    return $ req `satisfies` providerReq)
                                            valsArch
     where
        -- Return a versioned DepRequirement expression for this group id
        groupIdToDep :: MonadIO m => T.Text -> Key Groups -> SqlPersistT m DepRequirement
        groupIdToDep name groupId = do
            epochStr <- getValueForGroup groupId (TextKey "epoch")
            version  <- fromMaybe "" <$> getValueForGroup groupId (TextKey "version")
            release  <- fromMaybe "" <$> getValueForGroup groupId (TextKey "release")

            let epochInt = read <$> T.unpack <$> epochStr

            return $ DepRequirement name $ Just (RPM.EQ, EVR {epoch=epochInt, version, release})

    -- Check if the given group matches either the target arch or noarch
    matchesArch :: MonadIO m => Key Groups -> SqlPersistT m Bool
    matchesArch groupId = do
        kvArches <- mapMaybe keyValVal_value <$> getKeyValuesForGroup groupId (Just $ TextKey "arch")
        return $ (not . null) (("noarch":arches) `intersect` kvArches)

    -- various ways of converting things to DepRequirement

    -- convert the Either ParseError result from parseDepRequirement to a MonadError String
    parseDepRequirementError :: MonadError String m => T.Text -> m DepRequirement
    parseDepRequirementError req = either (throwError . show) return $ parseDepRequirement req

    -- key/val to DepRequirement, for rpm-provide/rpm-confict/rpm-obsolete values
    kvToDep :: MonadError String m => KeyVal -> m DepRequirement
    kvToDep KeyVal {keyValExt_value=Nothing} = throwError "Invalid key/val data"
    kvToDep KeyVal {keyValExt_value=Just ext} = parseDepRequirementError ext

    -- Requirement to DepRequirement
    reqToDep :: MonadError String m => Requirements -> m DepRequirement
    reqToDep Requirements{..} = parseDepRequirementError requirementsReq_expr
