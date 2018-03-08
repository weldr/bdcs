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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module BDCS.Export.FSTree(FSEntry,
                          FSTree,
                          filesToTree,
                          fstreeSource)
 where

import           Control.Conditional(whenM)
import           Control.Monad(foldM)
import           Control.Monad.Except(MonadError, throwError)
import           Control.Monad.State(StateT, evalStateT, get, withStateT)
import           Data.Conduit(Sink, Source, yield)
import qualified Data.Conduit.List as CL
import           Data.List.Safe(init, last)
import qualified Data.Text as T
import           Data.Tree(Tree(..))
import           System.FilePath(joinPath, splitDirectories)
import           System.Posix.Files(directoryMode, fileTypeModes, intersectFileModes, symbolicLinkMode)
import           System.Posix.Types(FileMode)

import BDCS.DB(Files(..))

import Prelude hiding(init, last)

-- FSTree types
-- A filesystem entry consists of a path component, and maybe a Files object.
-- Nothing for the object is used to represent placeholder directories.
-- e.g., if adding "/a/b" to an empty tree, a placeholder will be created for "a"
-- to hold the "b" entry. If "/a" is later added to the tree, the placeholder will
-- be replaced with a Files object for /a.

-- | A single node within a file system tree. The pair is a single path component
-- (e.g., "c" for the node at "/a/b/c"), and maybe a Files object. Automatically
-- created parent directories will use Nothing as the snd element.
type FSEntry = (FilePath, Maybe Files)

-- | A tree of file system entries.
type FSTree = Tree FSEntry

filesToTree :: MonadError String m => Sink Files m FSTree
filesToTree =
 let
    -- Create an empty tree. This tree starts one level above "/"
    rootTree = Node{rootLabel=("", Nothing), subForest=[]}
 in
    CL.foldM addFileToTree rootTree

addFileToTree :: MonadError String m => FSTree -> Files -> m FSTree
addFileToTree root object = do
    let rootZipper = (root, [])
    let pathComponents = splitDirectories $ T.unpack $ filesPath object

    dirComponents <- maybe (throwError $ "Invalid path on " ++ show object) return $ init pathComponents
    lastComponent <- maybe (throwError $ "Invalid path on " ++ show object) return $ last pathComponents

    -- Resolve the directory name
    dirZipper     <- evalStateT (findDirectory rootZipper dirComponents) 0

    -- Wrap the new file in a tree node and add it to the directory
    let newEntry = Node (lastComponent, Just object) []
    getTree <$> evalStateT (addEntryToTree dirZipper newEntry) 0
 where
    -- Given a directory path, split into components, return a new zipper focused on this path
    -- If the directory or any parent directories do not exist, create placeholders for them
    findDirectory :: MonadError String m => FSZipper -> [FilePath] -> StateT Int m FSZipper
    -- End of the list, we found it
    findDirectory zipper [] = return zipper

    -- . and .. cases
    findDirectory zipper (".":xs)  = findDirectory zipper xs
    findDirectory zipper ("..":xs) = findDirectory (goUp zipper) xs

    -- normal case: find the child entry, create if missing, recurse
    findDirectory zipper (pathComponent:xs) =
     let
        placeholder = Node (pathComponent, Nothing) []
     in
        -- Find this path component in the subForest for the current tree.
        -- Several things can happen:
        --    1) We find nothing. That's fine. Add a placeholder to the tree, recurse.
        --    2) We find a directory (or a placeholder). That's good. Recurse.
        --    3) We find a file. That's bad. Fail.
        --    4) We find a symlink. Oh boy!. Call findDirectory on the symlink target to
        --       get the actual directory we need to be in, then recurse.

        case findChild pathComponent zipper of
            -- No path found, add the placeholder and recurse
            Nothing -> findDirectory (addChild placeholder zipper) xs

            -- Grab the FSEntry out of the child tree, figure out what we found
            Just childZipper@(Node{..}, _) -> case categorize rootLabel of
                -- Found a placeholder, recurse
                Placeholder -> findDirectory childZipper xs

                -- existing directory, recurse
                Directory _ -> findDirectory childZipper xs

                -- symlink. Follow it, recurse on the symlink target
                -- check and increment the symlink level so we don't get stuck in a loop
                Symlink link -> do
                    whenM ((>= maxSymlinks) <$> get) $
                        throwError $ "Too many levels of symbolic links while resolving " ++ T.unpack (filesPath object)
                    linkZipper <- withStateT (+1) $ resolveSymlink zipper link
                    findDirectory linkZipper xs

                -- Anything else means there's a non-directory file in the middle of our path
                Other existing -> throwError $ "Unable to resolve path " ++ T.unpack (filesPath object) ++
                                               ", non-directory object exists at " ++ T.unpack (filesPath existing)

    -- Wrapper for findDirectory with a symlink target. If it's an absolute symlink target,
    -- unzip the zipper to the top and call findDirectory, otherwise just call findDirectory
    resolveSymlink :: MonadError String m => FSZipper -> Files -> StateT Int m FSZipper
    resolveSymlink zipper Files{..} = do
        symlinkTarget <- maybe (throwError $ "Error: symlink with no target at " ++ T.unpack filesPath)
                               (return . T.unpack) filesTarget
        let pathComponents = splitDirectories symlinkTarget
        let startZipper    = if head pathComponents == "/" then getRoot zipper else zipper

        findDirectory startZipper pathComponents

    addEntryToTree :: MonadError String m => FSZipper -> FSTree -> StateT Int m FSZipper
    addEntryToTree zipper newEntry = do
        -- At this point, we have a directory to stick the new entry into, and the first thing to do is see
        -- if the directory already contains an entry with the same name. If it does:
        --    - The new entry matches the old entry. That's fine, let everything be and we're done
        --    - The existing entry is a placeholder:
        --        * are we adding a directory? replace the placeholder with the new real directory, move the placeholder's
        --          children to the real directory
        --        * are we adding a symlink? replace the placeholder with the symlink, and move all of the placeholder's
        --          children to the symlink target
        --        * are we adding a different placeholder? Add the new placeholder's children as children of the existing
        --          placeholder.
        --        * otherwise? we have a conflict, throw an error
        --    - The existing entry is a directory:
        --        * are we adding a placeholder? Add the new placeholder's children to the existing directory.
        --        * otherwise, conflict
        --
        -- The scenarios that involve adding new placeholders may seem a little odd, but they can arise
        -- when moving things around due to replacing placeholder directories with symlinks.
        -- For example, adding something like the following to a tree:
        --
        -- 1) /a/b/c
        -- 2) /d/b/
        -- 3) /a -> d
        --
        -- would do something like:
        --
        -- 1) create placeholder /a, placeholder /a/b, real entry /a/b/c
        -- 2) create placeholder /d, real entry /d/b/
        -- 3) replace placeholder /a with a symlink, move placeholder /a/b/ and children to /d/,
        --    find that there's already a b/ entry and move /a/b/c to /d/b/

        let entryName = fst $ rootLabel newEntry
        let maybeExisting = findChild entryName zipper

        case maybeExisting of
            -- The easy case: nothing there, just add it
            Nothing       -> return $ addChild newEntry zipper

            Just existing@(self, crumbs) -> case ((categorize . rootLabel) self, (categorize . rootLabel) newEntry) of
                -- Move the new placeholder's children to the existing placeholder
                (Placeholder, Placeholder) -> addChildren existing newEntry
                -- Replace the existing placeholder, add its children to the new directory
                (Placeholder, Directory _) -> addChildren (newEntry, crumbs) self

                -- Replace the placeholder, find the symlink target, move the placeholder children to the target.
                -- The context for relative links is the directory, so one level above the new symlink
                (Placeholder, Symlink s)   -> do
                    let newZipper = (newEntry, crumbs)
                    targetZipper <- withStateT (+1) $ resolveSymlink (goUp newZipper) s
                    addChildren targetZipper self

                (Placeholder, _)           -> throwError $ "Unable to add " ++ T.unpack (filesPath object) ++
                                                           ", directory added at path"

                (Directory _, Placeholder) -> addChildren existing newEntry

                -- Allow this if the same Files object is being added twice
                (Directory d1, Directory d2) -> if compareDirs d1 d2 then addChildren existing newEntry
                                                else throwError $ "Unable to add " ++ T.unpack (filesPath object) ++
                                                                  ", file already added at this location"

                (Directory _, _)           -> throwError $ "Unable to to add " ++ T.unpack (filesPath object) ++
                                                           ", file already added at this location"

                -- Allow a symlink to be added twice if it's the same symlink
                (Symlink s1, Symlink s2)   -> if compareLinks s1 s2 then return existing
                                              else throwError $ "Unable to add " ++ T.unpack (filesPath object) ++
                                                                ", symlink already added at this location"

                -- Follow the symlink, and move the placeholder's children to the destination directory
                (Symlink s, Placeholder)   -> do
                    targetZipper <- withStateT (+1) $ resolveSymlink zipper s
                    addChildren targetZipper newEntry

                -- Otherwise, we have two non-directory, non-placeholder files, see if they match
                _                          -> if self == newEntry then return existing
                                              else throwError $ "Unable to add " ++ T.unpack (filesPath object) ++
                                                                ", file already added at this location"

    addChildren :: MonadError String m => FSZipper -> FSTree -> StateT Int m FSZipper
    addChildren dirZipper newEntry = foldM (\z e -> goUp <$> addEntryToTree z e) dirZipper (subForest newEntry)

    -- Compare files, ignoring size and mtime, because the data for these is basically made up
    compareDirs :: Files -> Files -> Bool
    compareDirs f1 f2 = f1{filesMtime=0, filesSize=0} == f2{filesMtime=0, filesSize=0}

    -- For symlinks, the only parts that count are the path and the target
    compareLinks :: Files -> Files -> Bool
    compareLinks f1 f2 = (filesPath f1 == filesPath f2) && (filesTarget f1 == filesTarget f2)

-- Walk a tree and emit the Files entries in order, modifying the paths as we go
-- to match the final, symlink-resolved results
fstreeSource :: Monad m => FSTree -> Source m Files
fstreeSource root = fstreeSource' [] root
 where
    fstreeSource' :: Monad m => [FilePath] -> FSTree -> Source m Files
    fstreeSource' prefix Node{rootLabel=(pathComponent, maybeFile), ..} =
        let newPrefix = prefix ++ [pathComponent]
        in yieldEntry (joinPath newPrefix) maybeFile >>
           mapM_ (fstreeSource' newPrefix) subForest

    -- yieldEntry :: Monad m => FilePath -> Maybe Files
    yieldEntry _ Nothing = return ()
    yieldEntry realPath (Just f) = yield f{filesPath=T.pack realPath}


-- Private symbols
-- The FSZipper is the currently focused node of the tree, plus a trail of breadcrumbs leading
-- back up to the top of the tree. Each crumb contains the rootLabel of the parent of the focused
-- tree, the subForest to the left of the focused tree, and the subForest to the right of the
-- focused tree.
type FSCrumb = (FSEntry, [FSTree], [FSTree])
type FSZipper = (FSTree, [FSCrumb])

-- Zipper navigation
-- Change the focus to the parent of the current tree
goUp :: FSZipper -> FSZipper
-- Pop the head off of the breadcrumb trail, use it to reconstruct the parent Tree
goUp (self, (entry, left, right):crumbs) = (Node entry (left ++ [self] ++ right), crumbs)
-- already at the top, just stay at the top
goUp zipper@(_, []) = zipper

-- Change the focus all the way to the top of a zipper
getRoot :: FSZipper -> FSZipper
getRoot zipper@(_, []) = zipper
getRoot zipper = getRoot $ goUp zipper

-- Convert a zipper back into a tree
getTree :: FSZipper -> FSTree
getTree zipper = fst $ getRoot zipper

-- Attempt to focus on a child of the current tree that matches the given path component
-- If there is no such child, return nothing
findChild :: FilePath -> FSZipper -> Maybe FSZipper
findChild pathComponent (self, crumbs) =
    case break ((== pathComponent) . fst . rootLabel) $ subForest self of
        -- An empty right list means no node was found
        (_, []) -> Nothing
        -- If we did find something, the head of the right list is the new tree to focus on
        (left, node:right) -> let newCrumb = (rootLabel self, left, right)
                               in Just (node, newCrumb:crumbs)

-- Add a new subtree to the currently focused tree, return a zipper focused on the new subtree
addChild :: FSTree -> FSZipper -> FSZipper
addChild subTree (Node{..}, crumbs) =
    let newCrumb = (rootLabel, subForest, [])
     in (subTree, newCrumb:crumbs)

-- Split FSEntries into categories, to simplify the conditionals involved in adding things to a FSTree
data FSCategory = Placeholder
                | Directory Files
                | Symlink Files
                | Other Files

categorize :: FSEntry -> FSCategory
categorize (_, Nothing) = Placeholder
categorize (_, Just f@Files{..}) =
    if | isDirectory -> Directory f
       | isSymlink   -> Symlink f
       | otherwise   -> Other f
 where
    getFileMode :: FileMode
    getFileMode = fromIntegral filesMode `intersectFileModes` fileTypeModes

    isDirectory :: Bool
    isDirectory = getFileMode == directoryMode

    isSymlink :: Bool
    isSymlink = getFileMode == symbolicLinkMode

-- Constant for detecting symlink loops, equivalent to MAXSYMLINKS in linux
maxSymlinks :: Int
maxSymlinks = 40
