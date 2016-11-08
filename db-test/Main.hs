{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Conduit(MonadResource, awaitForever, runResourceT, sourceFile)
import           Control.Conditional(notM, whenM)
import           Control.Exception(catch)
import           Control.Monad(void, when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.ByteString.Char8(pack)
import           Data.Conduit(($$), (=$=), Consumer, Producer)
import           Data.Maybe(fromMaybe, listToMaybe)
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Data.Word(Word16, Word32)
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.FilePath.Posix((</>))
import           System.IO(hPutStrLn, stderr)

import           BDCS.DB
import           BDCS.Exceptions
import           BDCS.FileType(getFileType)
import           BDCS.Projects(insertProject)
import qualified BDCS.ReqType as RT
import           RPM.Parse(parseRPMC)
import           RPM.Tags
import           RPM.Types

type FileTuple = (String, String, Int, String, String, Int, Int, Maybe String)

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
    throwIfNothingOtherwise sourceVersion (DBException "No Version tag") $ \v ->
        findSource v projectId >>= \case
            Nothing  -> insert $ mkSource rpm `throwIfNothing` DBException "Couldn't make Sources record"
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
    throwIfNothingOtherwise (era rpm) (DBException "No Epoch/Release/Arch tag") $ \(e, r, a) ->
        findBuild e r a sourceId >>= \case
            Nothing  -> insert $ mkBuild rpm `throwIfNothing` DBException "Couldn't make Builds record"
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

insertBuildSignatures :: MonadIO m => [Tag] -> Key Builds -> SqlPersistT m [Key BuildSignatures]
insertBuildSignatures sigs buildId =
    case (mkRSASignature sigs, mkSHASignature sigs) of
        (Just rsa, Just sha) -> mapM insert [rsa, sha]
        _                    -> return []
 where
    mkRSASignature :: [Tag] -> Maybe BuildSignatures
    mkRSASignature tags = do
        rsaSig <- findTag "RSAHeader" tags >>= \t -> tagValue t :: Maybe BS.ByteString
        return $ BuildSignatures buildId "RSA" rsaSig

    mkSHASignature :: [Tag] -> Maybe BuildSignatures
    mkSHASignature tags = do
        shaSig <- findTag "SHA1Header" tags >>= \t -> (tagValue t :: Maybe String) >>= Just . pack
        return $ BuildSignatures buildId "SHA1" shaSig

--
-- FILES
--

insertFiles :: MonadIO m => [Tag] -> SqlPersistT m [Key Files]
insertFiles rpm =
    mapM (insert . mkOneFiles)
         (zipFiles rpm)
 where
    mkOneFiles :: FileTuple -> Files
    mkOneFiles (path, digest, mode, user, group, size, mtime, target) =
        Files path digest (getFileType mode) mode user group size mtime target

    filePaths :: [Tag] -> [String]
    filePaths tags = let
        indexes   = fromMaybe [] $ findTag "DirIndexes" tags >>= \t -> tagValue t :: Maybe [Word32]
        dirnames  = findStringListTag "DirNames" tags
        basenames = findStringListTag "BaseNames" tags
     in
        zipWith (</>) (map (\i -> dirnames !! fromIntegral i) indexes) basenames

    zipFiles :: [Tag] -> [FileTuple]
    zipFiles tags = let
        megazip :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [(a, b, c, d, e, f, g, h)]
        megazip (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) = (a, b, c, d, e, f, g, h) : megazip as bs cs ds es fs gs hs
        megazip _ _ _ _ _ _ _ _ = []

        strToMaybe s = if s == "" then Nothing else Just s

        paths   = filePaths tags
        digests = findStringListTag "FileMD5s" tags
        modes   = fromMaybe [] $ findTag "FileModes" tags     >>= \t -> (tagValue t :: Maybe [Word16]) >>= Just . map fromIntegral
        users   = findStringListTag "FileUserName" tags
        groups  = findStringListTag "FileGroupName" tags
        sizes   = fromMaybe [] $ findTag "FileSizes" tags     >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
        mtimes  = fromMaybe [] $ findTag "FileMTimes" tags    >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
        targets = fromMaybe [] $ findTag "FileLinkTos" tags   >>= \t -> (tagValue t :: Maybe [String]) >>= Just . map strToMaybe
     in
        megazip paths digests modes users groups sizes mtimes targets

associateFilesWithBuild :: MonadIO m => [Key Files] -> Key Builds -> SqlPersistT m [Key BuildFiles]
associateFilesWithBuild files build =
    mapM (\(fID, bID) -> insert $ BuildFiles bID fID)
         (zip files $ repeat build)

associateFilesWithPackage :: MonadIO m => [Key Files] -> Key KeyVal -> SqlPersistT m [Key FileKeyValues]
associateFilesWithPackage files package =
    mapM (\(fID, pID) -> insert $ FileKeyValues fID pID)
         (zip files $ repeat package)

associateBuildWithPackage :: MonadIO m => Key Builds -> Key KeyVal -> SqlPersistT m (Key BuildKeyValues)
associateBuildWithPackage buildId kvId =
    insert $ BuildKeyValues buildId kvId

createGroup :: MonadIO m => [Key Files] -> [Tag] -> SqlPersistT m (Key Groups)
createGroup fileIds tags = do
    -- Get the NEVRA so it can be saved as attributes
    -- FIXME epoch, ignoring right now since it's optional, also using fromMaybe here seems probably bad
    let name = fromMaybe "" $ findStringTag "Name" tags
    let version = fromMaybe "" $ findStringTag "Version" tags
    let release = fromMaybe "" $ findStringTag "Release" tags
    let arch = fromMaybe "" $ findStringTag "Arch" tags

    -- Create the groups row
    groupId <- insert $ Groups name "rpm"

    -- Create the group_files rows
    void $ mapM (\fId -> insert $ GroupFiles groupId fId) fileIds

    -- Create the (E)NVRA attributes
    -- FIXME could at least deduplicate name and arch real easy
    void $ insertKeyValue "name" name >>= \kvId -> insert $ GroupKeyValues groupId kvId
    void $ insertKeyValue "version" version >>= \kvId -> insert $ GroupKeyValues groupId kvId
    void $ insertKeyValue "release" release >>= \kvId -> insert $ GroupKeyValues groupId kvId
    void $ insertKeyValue "arch" arch >>= \kvId -> insert $ GroupKeyValues groupId kvId

    -- Create the Provides attributes
    -- TODO versions, flags
    void $ mapM (\provide -> insertKeyValue "rpm-provide" provide >>= \kvId -> insert $ GroupKeyValues groupId kvId)
                (findStringListTag "ProvideName" tags)

    -- Create the requirements
    -- TODO versions, flags
    reqIdList <- mapM (\reqName -> insert $ Requirements RT.RPM RT.Runtime RT.Must reqName)
                      (findStringListTag "RequireName" tags)
    void $ mapM (\reqId -> insert $ GroupRequirements groupId reqId) reqIdList

    return groupId

--
-- KEY/VALUE
--

insertKeyValue :: MonadIO m => String -> String -> SqlPersistT m (Key KeyVal)
insertKeyValue k v =
    insert (KeyVal k v)

--
-- PACKAGES
--

-- select files.path
-- from files,key_val,file_key_values
-- on key_val.id == file_key_values.key_val_id and
--    file_key_values.file_id == files.id
-- where key_val.key_value == "packageName" and
--       key_val.val_value == "python3-kickstart"
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
    throwIfNothingOtherwise packageName (DBException "No Name tag") $ \name ->
        findPackage name >>= \case
            Nothing -> insertKeyValue "packageName" name
            Just p  -> return p
 where
    packageName = findStringTag "Name" rpm

    findPackage :: MonadIO m => String -> SqlPersistT m (Maybe (Key KeyVal))
    findPackage name = do
        ndx <- select $ from $ \pkg -> do
               where_ (pkg ^. KeyValKey_value ==. val "packageName" &&.
                       pkg ^. KeyValVal_value ==. val name)
               limit 1
               return (pkg ^. KeyValId)
        return $ listToMaybe (map unValue ndx)

--
-- WORKING WITH RPMS
--

loadRPM :: RPM -> IO ()
loadRPM RPM{..} = runSqlite "test.db" $ whenM (notM $ buildImported sigs) $ do
    projectId <- insertProject tags
    sourceId  <- insertSource tags projectId
    buildId   <- insertBuild tags sourceId
    void $ insertBuildSignatures sigs buildId
    filesIds  <- insertFiles tags
    pkgNameId <- insertPackageName tags

    void $ associateFilesWithBuild filesIds buildId
    void $ associateFilesWithPackage filesIds pkgNameId
    void $ associateBuildWithPackage buildId pkgNameId

    -- groups and reqs
    -- groupId <- createGroup filesIds tags
    void $ createGroup filesIds tags
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

buildImported :: MonadIO m => [Tag] ->  SqlPersistT m Bool
buildImported sigs =
    case findStringTag "SHA1Header" sigs of
        Just sha -> do ndx <- select $ from $ \signatures -> do
                              where_ (signatures ^. BuildSignaturesSignature_type ==. val "SHA1" &&.
                                      signatures ^. BuildSignaturesSignature_data ==. val (pack sha))
                              return (signatures ^. BuildSignaturesId)
                       return $ not $ null ndx
        Nothing  -> return False

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
    processOne path = catch (processRPM path >> putStrLn ("Imported " ++ path))
                            (\(e :: DBException) -> void $ hPutStrLn stderr ("*** Error importing RPM " ++ path ++ ": " ++ show e))
