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

module Main(main)
 where

import Data.List(isPrefixOf)
import Distribution.PackageDescription(Executable(..), GenericPackageDescription, HookedBuildInfo, PackageDescription(..))
import Distribution.Simple(defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Install(install)
import Distribution.Simple.InstallDirs(InstallDirs(..), fromPathTemplate, toPathTemplate)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo(..))
import Distribution.Simple.UserHooks(UserHooks(..))
import Distribution.Simple.Setup(ConfigFlags, CopyFlags)
import System.FilePath((</>))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { confHook = bdcsConf,
                                                copyHook = bdcsCopy }

-- Make sure $libexecdir gets "weldr/" appended to it.  This will affect both the installation location
-- for anything that should go into $libexecdir (like our subcommand programs) as well as the auto
-- generated Paths_bdcs.hs file.
--
-- This also means if you pass --libexecdir= to "cabal configure" or any other program, you should not
-- add "weldr/" to it.  Otherwise you'll get it in there twice.
bdcsConf :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
bdcsConf (descr, hbi) flags = do
    lbi <- confHook simpleUserHooks (descr, hbi) flags
    let dest = mangle $ libexecdir $ installDirTemplates lbi
    return $ lbi { installDirTemplates = (installDirTemplates lbi) { libexecdir = dest } }
 where
    mangle = toPathTemplate . (</> "weldr") . fromPathTemplate

bdcsCopy :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
bdcsCopy pkg lbi _ flags = do
    -- First, install everything that's not a subcommand.  That is almost everything
    -- listed in the cabal file.
    let (mainPkg, mainLbi) = extractMainComponents pkg lbi
    install mainPkg mainLbi flags

    -- Second, install only the subcommands into $prefix/libexec/weldr.
    let (subPkg, subLbi) = extractSubComponents pkg lbi
    install subPkg subLbi flags

isSubcommand :: String -> Bool
isSubcommand s = "inspect-" `isPrefixOf` s

extractMainComponents :: PackageDescription -> LocalBuildInfo -> (PackageDescription, LocalBuildInfo)
extractMainComponents pkg lbi = let
    pkg' = pkg { executables = filter (not . isSubcommand . exeName)
                                      (executables pkg) }
 in
    (pkg', lbi)

extractSubComponents :: PackageDescription -> LocalBuildInfo -> (PackageDescription, LocalBuildInfo)
extractSubComponents pkg lbi = let
    dest = libexecdir (installDirTemplates lbi)

    pkg' = pkg { executables = filter (isSubcommand . exeName)
                                      (executables pkg) }
    lbi' = lbi { installDirTemplates = (installDirTemplates lbi) { bindir = dest } }
 in
    (pkg', lbi')
