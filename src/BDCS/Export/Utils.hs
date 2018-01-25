-- |
-- Module: BDCS.Export.Utils
-- Copyright: (c) 2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- Miscellaneous utilities useful in exporting objects.

module BDCS.Export.Utils(runHacks,
                         runTmpfiles)
 where

import Control.Conditional(whenM)
import Control.Exception(tryJust)
import Control.Monad(guard)
import Data.List(intercalate)
import Data.List.Split(splitOn)
import System.Directory(createDirectoryIfMissing, doesFileExist, listDirectory, removePathForcibly, renameFile)
import System.FilePath((</>))
import System.IO.Error(isDoesNotExistError)
import System.Process(callProcess)

import BDCS.Export.TmpFiles(setupFilesystem)

import Paths_bdcs(getDataFileName)

-- | Run filesystem hacks needed to make a directory tree bootable.  Any exporter that produces a
-- finished image should call this function.  Otherwise, it is not generally useful and should be
-- avoided.  The exact hacks required is likely to change over time.
runHacks :: FilePath -> IO ()
runHacks exportPath = do
    -- set a root password
    -- pre-crypted from "redhat"
    shadowRecs <- map (splitOn ":") <$> lines <$> readFile (exportPath </> "etc" </> "shadow")
    let newRecs = map (\rec -> case rec of
                                   "root":_:rest -> ["root", "$6$3VLMX3dyCGRa.JX3$RpveyimtrKjqcbZNTanUkjauuTRwqAVzRK8GZFkEinbjzklo7Yj9Z6FqXNlyajpgCdsLf4FEQQKH6tTza35xs/"] ++ rest
                                   _             -> rec)
                      shadowRecs
    writeFile (exportPath </> "etc" </> "shadow.new") (unlines $ map (intercalate ":") newRecs)
    renameFile (exportPath </> "etc" </> "shadow.new") (exportPath </> "etc" </> "shadow")

    -- create an empty machine-id
    writeFile (exportPath </> "etc" </> "machine-id") ""

    -- Install a sysusers.d config file, and run systemd-sysusers to implement it
    let sysusersDir = exportPath </> "usr" </> "lib" </> "sysusers.d"
    createDirectoryIfMissing True sysusersDir
    getDataFileName "sysusers-default.conf" >>= readFile >>= writeFile (sysusersDir </> "weldr.conf")
    callProcess "systemd-sysusers" ["--root", exportPath]

    -- Run depmod on any kernel modules that might be present
    let modDir = exportPath </> "usr" </> "lib" </> "modules"
    modVers <- tryJust (guard . isDoesNotExistError) (listDirectory modDir)
    mapM_ (\ver -> callProcess "depmod" ["-b", exportPath, "-a", ver]) $ either (const []) id modVers

    -- Create a fstab stub
    writeFile (exportPath </> "etc" </> "fstab") "LABEL=composer / ext2 defaults 0 0"

    -- Clean up /run
    -- Some packages create directories in /var/run, which a symlink to /run, which is a tmpfs.
    (map ((exportPath </> "run") </>) <$> listDirectory (exportPath </> "run")) >>= mapM_ removePathForcibly

    -- EXTRA HACKY: turn off mod_ssl
    let sslConf = exportPath </> "etc" </> "httpd" </> "conf.d" </> "ssl.conf"
    whenM (doesFileExist sslConf)
          (renameFile sslConf (sslConf ++ ".off"))

-- | Run tmpfiles.d snippet on the new directory.  Most exporters should call this function.  Otherwise,
-- it is not generally useful and should be avoided.
runTmpfiles :: FilePath -> IO ()
runTmpfiles exportPath = do
    configPath <- getDataFileName "tmpfiles-default.conf"
    setupFilesystem exportPath configPath
