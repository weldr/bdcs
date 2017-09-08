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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Import.NPM(loadFromURI)
 where

import qualified Codec.Archive.Tar as Tar
import           Control.Monad(void)
import           Control.Monad.Catch(MonadThrow)
import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader(ReaderT, ask)
import           Control.Monad.State(execStateT)
import           Control.Monad.Trans.Resource(MonadBaseControl)
import           Data.Aeson(FromJSON(..), Object, Value(..), (.:), (.:?), (.!=), eitherDecode, withObject, withText)
import           Data.Aeson.Types(Parser, typeMismatch)
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit((.|), runConduitRes)
import           Data.Conduit.Binary(sinkLbs)
import qualified Data.HashMap.Lazy as HM
import           Data.List(isPrefixOf)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Database.Persist.Sql(SqlPersistT)
import           GI.Gio(noCancellable)
import           GI.OSTree(IsRepo, repoRegenerateSummary)
import           Network.URI(URI(..), URIAuth(..), nullURI, parseURI, relativeTo)
import           System.FilePath((</>), makeRelative, normalise, takeDirectory, takeFileName)
import           System.IO.Temp(withSystemTempDirectory)
import           Text.Regex.PCRE((=~))

import BDCS.CS(commit, commitContents, commitContentToFile, storeDirectory, withTransaction)
import BDCS.DB
import BDCS.Files(associateFilesWithSource, insertFiles)
import BDCS.KeyType
import BDCS.Projects(insertProject)
import BDCS.Sources(insertSource, insertSourceKeyValue)
import Build.NPM(rebuildNPM)
import Import.Conduit(getFromURI, ungzipIfCompressed)
import Import.State(ImportState(..))
import Utils.Either(whenLeft)
import Utils.Monad((>>?))

-- base URI for the package.json information
npmRegistry :: URI
npmRegistry = URI {uriScheme    = "https:",
                   uriAuthority = Just URIAuth{uriUserInfo = "", uriRegName = "registry.npmjs.org", uriPort = ""},
                   uriPath      = "/",
                   uriQuery     = "",
                   uriFragment  = "" }

-- The data returned by the registry contains a lot of the same things as package.json,
-- but it may not actually match the real data in package.json. The only part we can
-- use is information in the "dist" object.
data PackageDist = PackageDist {
    _integrity :: Maybe T.Text,
    _shasum :: Maybe T.Text,
    tarball :: T.Text }
 deriving(Show)

instance FromJSON PackageDist where
    parseJSON = withObject "registry JSON" $ \v ->
        case HM.lookup "dist" v of
            Nothing         -> fail "Missing dist object"
            Just (Object d) -> PackageDist <$> d .:? "integrity"
                                           <*> d .:? "shasum"
                                           <*> d .:  "tarball"
            Just _          -> fail "Dist not an object"

-- the parts of package.json we care about
data PackageJSON = PackageJSON {
    packageName :: T.Text,
    packageVersion :: T.Text,
    description :: T.Text,
    homepage :: Maybe T.Text,
    license :: T.Text,

    -- This can show up in package.json in two ways: either as the map of executable
    -- names to js paths ("bin" : { "exec1": "./script1.js", "exec2": "./script2.js" }),
    -- or as a single string ("bin": "./script1.js"). The single string case should be
    -- interpreted as the path to an executable that should be named the same as the name
    -- of the package.
    bin :: Maybe [(T.Text, T.Text)],

    -- This can appear as either a list of strings or a single string
    man :: Maybe [T.Text],

    -- The package.json documentation implies that it is an error to have both bin and
    -- directories.bin in package.json. And then npm itself does exactly that. So, what
    -- this actually means:
    -- * If bin is present, in any form (even if it is empty), it takes
    --   precedence and directories.bin is ignored for our purposes
    -- * If bin is not present and directories.bin is present, every path in this directory
    --   gets symlinked to /usr/bin. Subdirectories gets symlinked too and are not traversed,
    --   so if you have <bindir>/subdir, that gets symlinked as /usr/bin/subdir.
    binDirectory :: Maybe T.Text,

    -- Similar to bin, if man is present (even as an empty list), then this is ignored.
    -- Subdirectories are traversed.
    manDirectory :: Maybe T.Text,

    -- list of packagename, semver pairs
    dependencies :: Maybe [(T.Text, T.Text)] }
 deriving(Show)

instance FromJSON PackageJSON where
    parseJSON = withObject "package.json" $ \v -> PackageJSON
        <$> v .:  "name"
        <*> v .:  "version"
        <*> v .:? "description" .!= ""
        <*> v .:? "homepage"
        <*> v .:? "license"     .!= ""
        <*> parseBin v
        <*> v .:? "man"
        <*> parseDirectories v "bin"
        <*> parseDirectories v "man"
        <*> ((v .:? "dependencies") >>? parseTextObject)
     where
        -- custom handler for directories.bin and directories.man, to get rid of the intermediate object,
        -- and to skip if it's overriden by bin or man.
        parseDirectories :: Object -> T.Text -> Parser (Maybe T.Text)
        parseDirectories obj key =
            if HM.member key obj then return Nothing
            else case HM.lookup "directories" obj of
                Nothing         -> return Nothing
                Just (Object v) -> v .:? key
                Just err        -> typeMismatch "Object" err

        -- parse "bin", which has a mixed type
        parseBin :: Object -> Parser (Maybe [(T.Text, T.Text)])
        parseBin obj = do
            -- retrieve the name for the String case
            name <- (obj .: "name") >>= withText "String" return

            case HM.lookup "bin" obj of
                Nothing           -> return Nothing
                -- list of strings, return as a list of pairs
                Just v@(Object _) -> Just <$> parseTextObject v
                -- just a String, pair with the package name
                Just (String s)   -> return $ Just [(name, s)]
                Just err          -> typeMismatch "Object or String" err

        -- Convert an object that's all "key":"value" pairs to a list of (Text, Text)
        parseTextObject :: Value -> Parser [(T.Text, T.Text)]
        parseTextObject = withObject "Object" $ HM.foldrWithKey f (return [])
         where
            f :: T.Text -> Value -> Parser [(T.Text, T.Text)] -> Parser [(T.Text, T.Text)]
            f key val acc = withText "String" (\s -> ((key, s):) <$> acc) val

readRegistryJSON :: (MonadError String m, MonadBaseControl IO m, MonadThrow m, MonadIO m) => String -> m PackageDist
readRegistryJSON pkgname = do
    let uri = relativeTo (nullURI {uriPath = pkgname ++ "/latest"}) npmRegistry
    jsonData <- runConduitRes $ getFromURI uri .| sinkLbs
    either throwError return $ eitherDecode jsonData

importNPMDirToCS :: (MonadError String m, MonadIO m, IsRepo a) => a -> FilePath -> m (T.Text, PackageJSON)
importNPMDirToCS repo path = do
    -- Parse the package.json file
    jsonData <- liftIO $ BSL.readFile $ path </> "package.json"
    json <- either throwError return $ eitherDecode jsonData

    liftIO $ withTransaction repo $ \r -> do
        f <- storeDirectory r path
        c <- commit r f (T.concat ["Import of NPM package ", packageName json, "@", packageVersion json, " into the repo"]) Nothing
        return (c, json)

loadIntoMDDB :: MonadIO m => PackageJSON -> [Files] -> SqlPersistT m (Key Sources)
loadIntoMDDB PackageJSON{..} files = do
    -- Create the project/source/build entries from the package.json data
    -- npm doesn't provide separate descriptions and summaries, so just leave projects.description blank
    -- upstream_vcs is usually the same as homepage, but we can't tell automatically so leave that blank too
    projectId <- insertProject $ Projects packageName description "" homepage ""
    sourceId  <- insertSource $ Sources projectId license packageVersion ""

    fileIds <- insertFiles files
    void $ associateFilesWithSource fileIds sourceId

    -- load the bin information into the mddb as key/val pairs. /usr/bin symlinks will not be created
    -- until export, since there could be multiple versions of a package installed as dependencies and
    -- we only want the bin symlinks for the top-level ones.
    -- If there is an explicit bin list, that takes precendence, otherwise use directories.bin.
    case (bin, binDirectory) of
        (Just binlist, _)      -> mapM_ (addBin sourceId) binlist
        (Nothing, Just binDir) -> addBinDir sourceId binDir
        _                      -> return ()

    -- similar thing for man pages
    case (man, manDirectory) of
        (Just manList, _)      -> mapM_ (addMan sourceId) manList
        (Nothing, Just manDir) -> addManDir sourceId manDir
        _                      -> return ()

    -- save the requirements as build key/vals. These are the semver requirement ranges.
    -- When the source is "linked" into a build, and from to an exportable group, the semvers
    -- will be translated to exact-version requirements and stored in the requirements table.
    mapM_ (\(reqname, reqver) -> insertSourceKeyValue (TextKey "dependency") reqname (Just reqver) sourceId) $
        fromMaybe [] dependencies

    -- mark the source as coming from npm
    -- TODO figure out a better way to express this kind of thing
    void $ insertSourceKeyValue (TextKey "npm") "" Nothing sourceId

    return sourceId
 where
    normaliseText :: T.Text -> T.Text
    normaliseText path = T.pack $ normalise $ T.unpack path

    -- package.json contains "bin", which is a list of (<bin name>, <path>) pairs
    -- Insert as k=bin, v=<binname>, e=<path>
    addBin :: MonadIO m => Key Sources -> (T.Text, T.Text) -> SqlPersistT m ()
    addBin sourceId (binName, path) = void $ insertSourceKeyValue (TextKey "bin") binName (Just (normaliseText path)) sourceId

    -- package.json contains "directories.bin", which means everything in that directory
    -- should become a /usr/bin symlink, using the name of the file as the name of the symlink.
    -- No recursion, so if there's something like <bindir>/subdir/subpath, subdir gets a symlink
    -- and subpath is otherwise ignored.
    -- Create KeyVal values like in addBin, using the filename for v.
    addBinDir :: MonadIO m => Key Sources -> T.Text -> SqlPersistT m ()
    addBinDir sourceId binDir = let
        -- normalize out the leading "./" and any other funkiness
        binPrefix = normalise (T.unpack binDir)

        -- find paths where the directory component is the same as the prefix
        binFiles = filter (\p -> takeDirectory p == binPrefix) $ map (makeRelative "/" . T.unpack . filesPath) files
     in
        mapM_ (\p -> insertSourceKeyValue (TextKey "bin") (T.pack $ takeFileName p) (Just (T.pack p)) sourceId) binFiles

    addMan :: MonadIO m => Key Sources -> T.Text -> SqlPersistT m ()
    addMan sourceId manName = void $ insertSourceKeyValue (TextKey "man") (normaliseText manName) Nothing sourceId

    -- Unlike directories.bin, we do need to recurse into this directory
    addManDir :: MonadIO m => Key Sources -> T.Text -> SqlPersistT m ()
    addManDir sourceId manDir = let
        manPrefix = normalise (T.unpack manDir)
        paths = map (makeRelative "/" . T.unpack . filesPath) files
        manFiles = filter (\p -> (manPrefix `isPrefixOf` p) && (p =~ ("\\.[0-9]$" :: String))) paths
     in
        mapM_ (\p -> insertSourceKeyValue (TextKey "man") (T.pack p) Nothing sourceId) manFiles

loadFromURI :: URI -> ReaderT ImportState IO ()
loadFromURI uri@URI{..} = do
    db <- stDB <$> ask
    repo <- stRepo <$> ask

    -- run the whole thing with a temp directory
    result <- liftIO $ withSystemTempDirectory "npm-import" $ \path ->
        runExceptT $ do
            -- Fetch the JSON describing the package
            distJson <- readRegistryJSON uriPath

            -- Fetch the tarball
            let distTarball = T.unpack $ tarball distJson
            distURI <- maybe (throwError $ "Error parsing dist URI: " ++ distTarball) return $ parseURI distTarball
            distBytes <- runConduitRes $ getFromURI distURI .| ungzipIfCompressed .| sinkLbs

            -- TODO verify checksum

            -- unpack the tarball to a temp directory
            (liftIO . Tar.unpack path) $ Tar.read distBytes

            -- All of the files in a NPM tarball are in a "package/" directory,
            -- so use that as the root of the import.
            let packagePath = path </> "package"

            -- import the temp directory into the content store
            (commitChecksum, packageJson) <- importNPMDirToCS repo packagePath

            -- get the list of files we just imported into the content store
            checksums <- liftIO (execStateT (commitContents repo commitChecksum) [])

            -- gather the metadata for all of paths as Files DB objects
            files <- liftIO $ mapM (commitContentToFile packagePath) checksums

            -- import the npm data into the mddb
            checkAndRunSqlite (T.pack db) $ do
                sourceId <- loadIntoMDDB packageJson files
                void $ rebuildNPM repo sourceId

            -- regenerate the content store summary
            liftIO $ repoRegenerateSummary repo Nothing noCancellable

    whenLeft result (\e -> liftIO $ print $ "Error importing " ++ show uri ++ ": " ++ show e)
