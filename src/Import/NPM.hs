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

import           Control.Monad(void)
import           Control.Monad.Catch(MonadThrow)
import           Control.Monad.Except(MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Reader(ReaderT, ask)
import           Control.Monad.State(MonadState, get, modify)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadResource)
import           Data.Aeson(FromJSON(..), Object, Value(..), (.:), (.:?), (.!=), eitherDecode, withObject, withText)
import           Data.Aeson.Types(Parser, typeMismatch)
import           Data.Bits((.|.))
import           Data.ByteArray(convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit(Conduit, Consumer, ZipConduit(..), (.|), getZipConduit, runConduitRes, toConsumer, yield)
import           Data.Conduit.Binary(sinkLbs)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit.Lift(evalStateLC)
import qualified Data.Conduit.Tar as CT
import           Data.ContentStore(ContentStore, storeByteStringSink)
import           Data.ContentStore.Digest(ObjectDigest)
import qualified Data.HashMap.Lazy as HM
import           Data.List(isPrefixOf)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding(decodeUtf8)
import           Database.Persist.Sql(SqlPersistT)
import           Network.URI(URI(..), URIAuth(..), nullURI, parseURI, relativeTo)
import           System.FilePath((</>), makeRelative, normalise, takeDirectory, takeFileName)
import           System.Posix.Files(blockSpecialMode, characterSpecialMode, directoryMode, namedPipeMode, regularFileMode, symbolicLinkMode)
import           Text.Regex.PCRE((=~))

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
    --
    --   * If bin is present, in any form (even if it is empty), it takes
    --     precedence and directories.bin is ignored for our purposes
    --   * If bin is not present and directories.bin is present, every path in this directory
    --     gets symlinked to /usr/bin. Subdirectories gets symlinked too and are not traversed,
    --     so if you have <bindir>/subdir, that gets symlinked as /usr/bin/subdir.
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

    result <- runExceptT $ do
        -- Fetch the JSON describing the package
        distJson <- readRegistryJSON uriPath

        -- Get the URI to the tarball out of the JSON
        let distTarball = T.unpack $ tarball distJson
        distURI <- maybe (throwError $ "Error parsing dist URI: " ++ distTarball) return $ parseURI distTarball

        -- conduits for consuming the tar entries:
        -- this one loads the content into the content store and returns a list of (uninserted) Files records
        let pathPipe = tarEntryToFile repo .| CL.consume
        -- this one returns the parsed package.json
        let jsonPipe = parsePackageJson

        -- Zip them together into a sink returning a tuple
        let tarSink = getZipConduit ((,) <$> ZipConduit pathPipe
                                         <*> ZipConduit jsonPipe)

        -- Import the tarball to the content store
        (files, packageJson) <- runConduitRes $
                                         getFromURI distURI
                                      .| ungzipIfCompressed
                                      .| CT.untar
                                      .| tarSink

        -- Insert the metadata
        checkAndRunSqlite (T.pack db) $ do
            source <- loadIntoMDDB packageJson files

            -- Link the dependencies for the source
            rebuildNPM source

    whenLeft result (\e -> liftIO $ print $ "Error importing " ++ show uri ++ ": " ++ show e)

 where
    -- TODO handle TarExceptions
    tarEntryToFile :: (MonadError String m, MonadThrow m, MonadResource m) => ContentStore -> Conduit CT.TarChunk m Files
    tarEntryToFile cs =
        -- Run the tar processing in a state with a map from FilePath to (ObjectDigest, CT.Size),
        -- so hardlinks can get the data they need from earlier entries.
        evalStateLC HM.empty $ CT.withEntries handleEntry
     where
        handleEntry :: (MonadState (HM.HashMap FilePath (ObjectDigest, CT.Size)) m, MonadError String m, MonadResource m) => CT.Header -> Conduit BS.ByteString m Files
        handleEntry header@CT.Header{..} = do
            let entryPath = CT.headerFilePath header

            -- Ignore the mode from tar. Set everything to 0644 if it's a file, 0755 if it's a directory
            let modeBits = if CT.headerFileType header == CT.FTDirectory then 0o0755 else 0o0644

            -- Add the file type bits to the mode based on the tar header type
            let typeBits = case CT.headerFileType header of
                    CT.FTNormal           -> regularFileMode
                    CT.FTHardLink         -> regularFileMode
                    CT.FTSymbolicLink     -> symbolicLinkMode
                    CT.FTCharacterSpecial -> characterSpecialMode
                    CT.FTBlockSpecial     -> blockSpecialMode
                    CT.FTDirectory        -> directoryMode
                    CT.FTFifo             -> namedPipeMode
                    -- TODO?
                    CT.FTOther _          -> 0

            -- Make the start of a Files record. Ignore the user/group/mode from tar
            let baseFile = Files{filesPath       = T.pack ("/" </> normalise entryPath),
                                 filesFile_user  = "root",
                                 filesFile_group = "root",
                                 filesMtime      = fromIntegral headerTime,
                                 filesMode       = modeBits .|. fromIntegral typeBits,
                                 filesTarget     = Nothing,
                                 filesCs_object  = Nothing,
                                 filesSize       = 0}

            file <- case CT.headerFileType header of
                -- for NormalFile, add the object to the content store, add the digest and size to the state, and add the digest in the record
                CT.FTNormal       -> handleRegularFile baseFile entryPath headerPayloadSize
                -- For hard links, the content is the link target: look it up in the state and fill in the digest and size
                CT.FTHardLink     -> handleHardLink baseFile

                -- for symlinks, set the target
                CT.FTSymbolicLink -> handleSymlink baseFile

                -- TODO?
                CT.FTOther code   -> throwError $ "Unknown tar entry type " ++ show code

                -- TODO: need somewhere for block/char special major and minor
                -- otherwise nothing else has anything special
                _                 -> return baseFile

            yield file

        handleRegularFile :: (MonadState (HM.HashMap FilePath (ObjectDigest, CT.Size)) m, MonadError String m, MonadResource m) => Files -> FilePath -> CT.Size -> Consumer BS.ByteString m Files
        handleRegularFile baseFile entryPath size = do
            digest <- toConsumer $ storeByteStringSink cs
            modify (HM.insert entryPath (digest, size))
            return $ baseFile {filesSize      = fromIntegral size,
                               filesCs_object = Just $ convert digest}

        handleHardLink :: (MonadState (HM.HashMap FilePath (ObjectDigest, CT.Size)) m, MonadError String m) => Files -> Consumer BS.ByteString m Files
        handleHardLink baseFile = do
            -- Use the same ByteString -> FilePath unpacking that headerFilePath uses
            target <- S8.unpack <$> CC.fold
            (HM.lookup target <$> get) >>=
                maybe (throwError $ "Broken hard link to " ++ target)
                      (\(digest, size) -> return $ baseFile {filesSize      = fromIntegral size,
                                                             filesCs_object = Just $ convert digest})

        handleSymlink :: Monad m => Files -> Consumer BS.ByteString m Files
        handleSymlink baseFile = do
            target <- decodeUtf8 <$> CC.fold
            return $ baseFile {filesTarget = Just target}

    -- TODO handle TarExceptions
    parsePackageJson :: (MonadError String m, MonadThrow m) => Consumer CT.TarChunk m PackageJSON
    parsePackageJson = CT.withEntry handler >>= maybe parsePackageJson return
     where
        handler :: MonadError String m => CT.Header -> Consumer BS.ByteString m (Maybe PackageJSON)
        handler header = let
            path = makeRelative "/" $ normalise $ CT.headerFilePath header
         in
            -- Everything in an npm tarball is under "package/"
            if path == ("package" </> "package.json") then
                (eitherDecode <$> BSL.fromStrict <$> CC.fold) >>= either throwError (return . Just)
            else
                return Nothing
