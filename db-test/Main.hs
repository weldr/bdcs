{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Conduit(MonadResource, awaitForever, runResourceT, sourceFile)
import           Control.Exception(Exception, catch, throw)
import           Control.Monad(void, when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8(pack, unpack)
import           Data.Conduit(($$), (=$=), Consumer, Producer)
import           Data.Data(Typeable, cast, gmapQi, showConstr, toConstr)
import           Data.List(find, zip7)
import           Data.Maybe(fromJust, fromMaybe, listToMaybe)
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Data.Word(Word16, Word32)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath.Posix((</>))
import           System.IO(hPutStrLn, stderr)

import DB
import FileType(getFileType)
import RPM.Parse(parseRPMC)
import RPM.Tags
import RPM.Types

data DBException = DBException
 deriving(Show, Typeable)

instance Exception DBException

throwIfNothing :: Exception e => Maybe a -> e -> a
throwIfNothing (Just v) _   = v
throwIfNothing _        exn = throw exn

throwIfNothingOtherwise :: Exception e => Maybe a -> e -> (a -> b) -> b
throwIfNothingOtherwise (Just v) _   fn = fn v
throwIfNothingOtherwise _        exn _  = throw exn

findTag :: String -> [Tag] -> Maybe Tag
findTag name = find (\t -> name == showConstr (toConstr t))

findByteStringTag :: String -> [Tag] -> Maybe BS.ByteString
findByteStringTag name tags = findTag name tags >>= \t -> tagValue t :: Maybe BS.ByteString

findStringTag :: String -> [Tag] -> Maybe String
findStringTag name tags = findTag name tags >>= \t -> tagValue t :: Maybe String

findStringListTag :: String -> [Tag] -> [String]
findStringListTag name tags = fromMaybe [] $ findTag name tags >>= \t -> tagValue t :: Maybe [String]

tagValue :: Typeable a => Tag -> Maybe a
tagValue = gmapQi 0 cast

--
-- PROJECTS
--

findProject :: MonadIO m => String -> SqlPersistT m (Maybe (Key Projects))
findProject name = do
    ndx <- select $ from $ \proj -> do
           where_ (proj ^. ProjectsName ==. val name)
           limit 1
           return (proj ^. ProjectsId)
    return $ listToMaybe (map unValue ndx)

insertProject :: MonadIO m => [Tag] -> SqlPersistT m (Key Projects)
insertProject rpm =
    throwIfNothingOtherwise projectName DBException $ \n ->
        findProject n >>= \case
            Nothing   -> insert $ mkProject rpm `throwIfNothing` DBException
            Just proj -> return proj
 where
    mkProject :: [Tag] -> Maybe Projects
    mkProject tags = do
        name        <- projectName
        summary     <- findByteStringTag "Summary" tags >>= Just . unpack
        description <- findByteStringTag "Description" tags >>= Just . unpack
        homepage    <- findStringTag "URL" tags

        -- FIXME:  Where to get this from?
        let upstream_vcs = "UPSTREAM_VCS"

        return $ Projects name summary description homepage upstream_vcs

    -- FIXME:  SourceRPM looks like "pykickstart-2.32-1.fc26.src.rpm", but we really just want "pykickstart"
    -- here.  I'll get to that.
    projectName = findStringTag "SourceRPM" rpm

--
-- SOURCES
--

findSource :: MonadIO m => String -> Key Projects -> SqlPersistT m (Maybe (Key Sources))
findSource version projectId = do
    -- FIXME:  Is (project_id, version) unique in Sources?
    ndx <- select $ from $ \src -> do
           where_ (src ^. SourcesProject_id ==. val projectId &&.
                   src ^. SourcesVersion    ==. val version)
           limit 1
           return (src ^. SourcesId)
    return $ listToMaybe (map unValue ndx)

insertSource :: MonadIO m => [Tag] -> Key Projects -> SqlPersistT m (Key Sources)
insertSource rpm projectId =
    throwIfNothingOtherwise sourceVersion DBException $ \v ->
        findSource v projectId >>= \case
            Nothing  -> insert $ mkSource rpm `throwIfNothing` DBException
            Just src -> return src
 where
    mkSource :: [Tag] -> Maybe Sources
    mkSource tags = do
        license <- findStringTag "License" tags
        version <- sourceVersion
 
        -- FIXME:  Where to get this from?
        let source_ref = "SOURCE_REF"

        return $ Sources projectId license version source_ref

    sourceVersion = findStringTag "Version" rpm

--
-- BUILDS
--

findBuild :: MonadIO m => Int -> String -> String -> Key Sources -> SqlPersistT m (Maybe (Key Builds))
findBuild epoch release arch sourceId = do
    -- FIXME: Is (source_id, epoch, release, arch) unique in Builds?
    ndx <- select $ from $ \build -> do
           where_ (build ^. BuildsSource_id ==. val sourceId &&.
                   build ^. BuildsEpoch ==. val epoch &&.
                   build ^. BuildsRelease ==. val release &&.
                   build ^. BuildsArch ==. val arch)
           limit 1
           return (build ^. BuildsId)
    return $ listToMaybe (map unValue ndx)

insertBuild :: MonadIO m => [Tag] -> Key Sources -> SqlPersistT m (Key Builds)
insertBuild rpm sourceId =
    throwIfNothingOtherwise (era rpm) DBException $ \(e, r, a) ->
        findBuild e r a sourceId >>= \case
            Nothing  -> insert $ mkBuild rpm `throwIfNothing` DBException
            Just bld -> return bld
 where
    mkBuild :: [Tag] -> Maybe Builds
    mkBuild tags = do
        (epoch, release, arch) <- era tags
        build_time <- findTag "BuildTime"     tags >>= \t -> (tagValue t :: Maybe Word32)   >>= Just . posixSecondsToUTCTime . realToFrac
        -- FIXME: RPM splits the changelog up into three tag types.  I'm just grabbing the text here for now.
        changelog  <- findTag "ChangeLogText" tags >>= \t -> (tagValue t :: Maybe [String]) >>= Just . head >>= Just . pack

        -- FIXME:  Where to get these from?
        let build_config_ref = "BUILD_CONFIG_REF"
        let build_env_ref = "BUILD_ENV_REF"

        return $ Builds sourceId epoch release arch build_time changelog build_config_ref build_env_ref

    era :: [Tag] -> Maybe (Int, String, String)
    era tags = do
        epoch   <- maybe (Just 0) (\t -> (tagValue t :: Maybe Word32) >>= Just . fromIntegral) (findTag "Epoch" tags)
        release <- findStringTag "Release" tags
        arch    <- findStringTag "Arch" tags
        return (epoch, release, arch)

insertBuildSignature :: MonadIO m => [Tag] -> Key Builds -> SqlPersistT m (Maybe (Key BuildSignatures))
insertBuildSignature sigs buildId =
    case mkBuildSignature sigs of
        Just sig -> insert sig >>= return . Just
        Nothing  -> return Nothing
 where
    mkBuildSignature :: [Tag] -> Maybe BuildSignatures
    mkBuildSignature tags = do
        rsaSig <- findTag "RSAHeader" tags >>= \t -> tagValue t :: Maybe BS.ByteString
        return $ BuildSignatures buildId "RSA" rsaSig

--
-- FILES
--

insertFiles :: MonadIO m => [Tag] -> SqlPersistT m [Key Files]
insertFiles rpm =
    mapM (insert . mkOneFiles)
         (zipFiles rpm)
 where
    mkOneFiles :: (String, String, Int, String, String, Int, Maybe String) -> Files
    mkOneFiles (path, digest, mode, user, group, mtime, target) =
        Files path digest (getFileType mode) mode user group Nothing Nothing mtime target

    filePaths :: [Tag] -> [String]
    filePaths tags = let
        indexes   = fromMaybe [] $ findTag "DirIndexes" tags >>= \t -> tagValue t :: Maybe [Word32]
        dirnames  = findStringListTag "DirNames" tags
        basenames = findStringListTag "BaseNames" tags
     in
        zipWith (</>) (map (\i -> dirnames !! fromIntegral i) indexes) basenames

    zipFiles :: [Tag] -> [(String, String, Int, String, String, Int, Maybe String)]
    zipFiles tags = let
        strToMaybe s = if s == "" then Nothing else Just s

        maybeToList (Just l) = l
        maybeToList _        = []

        paths   = filePaths tags
        digests = findStringListTag "FileMD5s" tags
        modes   = maybeToList $ findTag "FileModes" tags     >>= \t -> (tagValue t :: Maybe [Word16]) >>= Just . map fromIntegral
        users   = findStringListTag "FileUserName" tags
        groups  = findStringListTag "FileGroupName" tags
        mtimes  = maybeToList $ findTag "FileMTimes" tags    >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
        targets = maybeToList $ findTag "FileLinkTos" tags   >>= \t -> (tagValue t :: Maybe [String]) >>= Just . map strToMaybe
     in
        zip7 paths digests modes users groups mtimes targets

associateFilesWithBuild :: MonadIO m => [Key Files] -> Key Builds -> SqlPersistT m [Key BuildFiles]
associateFilesWithBuild files build =
    mapM (\(fID, bID) -> insert $ BuildFiles bID fID)
         (zip files $ repeat build)

associateFilesWithPackage :: MonadIO m => [Key Files] -> Key KeyVal -> SqlPersistT m [Key FileKeyValues]
associateFilesWithPackage files package =
    mapM (\(fID, pID) -> insert $ FileKeyValues fID pID)
         (zip files $ repeat package)

--
-- KEY/VALUE
--

insertKeyValue :: MonadIO m => String -> String -> SqlPersistT m (Key KeyVal)
insertKeyValue k v =
    insert (KeyVal k v)

--
-- WORKING WITH RPMS
--

-- select files.path
-- from files,key_val,file_key_values
-- on key_val.id == file_key_values.key_val_id and
--    file_key_values.file_id == files.id;
-- where key_val.key_value == "packageName" and
--       key_val.val_value == "python3-kickstart" and
filesInPackage :: MonadIO m => String -> SqlPersistT m [String]
filesInPackage name = do
    results <- select $ from $ \(files `InnerJoin` key_val `InnerJoin` file_key_values) -> do
               on (key_val ^. KeyValId ==. file_key_values ^. FileKeyValuesKey_val_id &&.
                   file_key_values ^. FileKeyValuesFile_id ==. files ^. FilesId)
               where_ (key_val ^. KeyValKey_value ==. val "packageName" &&.
                       key_val ^. KeyValVal_value ==. val name)
               return (files ^. FilesPath)
    return $ map unValue results

insertPackageName :: MonadIO m => [Tag] -> SqlPersistT m (Key KeyVal)
insertPackageName rpm =
    throwIfNothingOtherwise packageName DBException $ \name ->
        findPackage name >>= \case
            [] -> insertKeyValue "packageName" name
            _  -> throw DBException
 where
    packageName = findStringTag "Name" rpm

    findPackage :: MonadIO m => String -> SqlPersistT m [Key KeyVal]
    findPackage name = do
        ndx <- select $ from $ \pkg -> do
               where_ (pkg ^. KeyValKey_value ==. val name)
               return (pkg ^. KeyValId)
        return (map unValue ndx)

loadRPM :: RPM -> IO ()
loadRPM RPM{..} = runSqlite "test.db" $ do
    projectId <- insertProject tags
    sourceId  <- insertSource tags projectId
    buildId   <- insertBuild tags sourceId
    void $ insertBuildSignature sigs buildId
    filesIds  <- insertFiles tags

    void $ associateFilesWithBuild filesIds buildId
    void $ insertPackageName tags >>= associateFilesWithPackage filesIds

    -- FIXME:  The following lines are just an example to show that things are working.
    -- This should be removed before really using this code.
    let name = fromJust $ findStringTag "Name" tags
    liftIO $ putStrLn $ "Loaded the following files from " ++ name ++ ":"
    filesInPackage name >>= mapM_ (liftIO . putStrLn)
 where
    -- FIXME:  Be less stupid.
    sigs = headerTags $ head rpmHeaders
    tags = headerTags $ rpmHeaders !! 1

processRPM :: FilePath -> IO ()
processRPM path = void $ runExceptT $ runResourceT pipeline
 where
    pipeline = getRPM path =$= parseRPMC $$ consumer

    getRPM :: MonadResource m => FilePath -> Producer m BS.ByteString
    getRPM = sourceFile

    consumer :: MonadIO m => Consumer RPM m ()
    consumer = awaitForever (liftIO . loadRPM)

--
-- MAIN
--

main :: IO ()
main = do
    -- Read the list of rpms to process from the command line arguments
    argv <- getArgs

    when (length argv < 1) $ do
        putStrLn "Usage: test RPM [RPM ...]"
        exitFailure

    initDB "test.db"
    mapM_ processOne argv
 where
    processOne path = catch (processRPM path)
                            (\(_ :: DBException) -> void $ hPutStrLn stderr ("Error importing RPM: " ++ path))
