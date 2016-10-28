{-# LANGUAGE FlexibleContexts #-}
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
import           Data.Maybe(fromMaybe)
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

findTag :: String -> [Tag] -> Maybe Tag
findTag name = find (\t -> name == showConstr (toConstr t))

tagValue :: Typeable a => Tag -> Maybe a
tagValue = gmapQi 0 cast

insertProject :: MonadIO m => [Tag] -> SqlPersistT m (Key Projects)
insertProject rpm =
    case mkProject rpm of
        Just proj -> insert proj
        Nothing   -> throw DBException
 where
    mkProject :: [Tag] -> Maybe Projects
    mkProject tags = do
        name        <- findTag "Name"        tags >>= \t -> tagValue t :: Maybe String
        summary     <- findTag "Summary"     tags >>= \t -> (tagValue t :: Maybe BS.ByteString) >>= Just . unpack
        description <- findTag "Description" tags >>= \t -> (tagValue t :: Maybe BS.ByteString) >>= Just . unpack
        homepage    <- findTag "URL"         tags >>= \t -> tagValue t :: Maybe String

        -- FIXME:  Where to get this from?
        let upstream_vcs = "UPSTREAM_VCS"

        return $ Projects name summary description homepage upstream_vcs

insertSource :: MonadIO m => [Tag] -> Key Projects -> SqlPersistT m (Key Sources)
insertSource rpm projectId =
    case mkSource rpm of
        Just source -> insert source
        Nothing     -> throw DBException
 where
    mkSource :: [Tag] -> Maybe Sources
    mkSource tags = do
        license <- findTag "License" tags >>= \t -> tagValue t :: Maybe String
        version <- findTag "Version" tags >>= \t -> tagValue t :: Maybe String
 
        -- FIXME:  Where to get this from?
        let source_ref = "SOURCE_REF"

        return $ Sources projectId license version source_ref

insertBuild :: MonadIO m => [Tag] -> Key Sources -> SqlPersistT m (Key Builds)
insertBuild rpm sourceId =
    case mkBuild rpm of
        Just build -> insert build
        Nothing    -> throw DBException
 where
    mkBuild :: [Tag] -> Maybe Builds
    mkBuild tags = do
        epoch      <- maybe (Just 0) (\t -> (tagValue t :: Maybe Word32) >>= Just . fromIntegral) (findTag "Epoch" tags)
        release    <- findTag "Release"       tags >>= \t -> tagValue t :: Maybe String
        arch       <- findTag "Arch"          tags >>= \t -> tagValue t :: Maybe String
        build_time <- findTag "BuildTime"     tags >>= \t -> (tagValue t :: Maybe Word32)   >>= Just . posixSecondsToUTCTime . realToFrac
        -- FIXME: RPM splits the changelog up into three tag types.  I'm just grabbing the text here for now.
        changelog  <- findTag "ChangeLogText" tags >>= \t -> (tagValue t :: Maybe [String]) >>= Just . head >>= Just . pack

        -- FIXME:  Where to get these from?
        let build_config_ref = "BUILD_CONFIG_REF"
        let build_env_ref = "BUILD_ENV_REF"

        return $ Builds sourceId epoch release arch build_time changelog build_config_ref build_env_ref

insertBuildSignature :: MonadIO m => [Tag] -> Key Builds -> SqlPersistT m (Maybe (Key BuildSignatures))
insertBuildSignature sigs buildId =
    case mkBuildSignature sigs of
        Just sig -> do i <- insert sig
                       return $ Just i
        Nothing  -> return Nothing
 where
    mkBuildSignature :: [Tag] -> Maybe BuildSignatures
    mkBuildSignature tags = do
        rsaSig <- findTag "RSAHeader" tags >>= \t -> tagValue t :: Maybe BS.ByteString
        return $ BuildSignatures buildId "RSA" rsaSig

insertFiles :: MonadIO m => [Tag] -> SqlPersistT m [Key Files]
insertFiles rpm = do
    let zipped = zipFiles rpm
    mapM (insert . mkOneFiles) zipped
 where
    mkOneFiles :: (String, String, Int, String, String, Int, Maybe String) -> Files
    mkOneFiles (path, digest, mode, user, group, mtime, target) =
        Files path digest (getFileType mode) mode user group Nothing Nothing mtime target

    filePaths :: [Tag] -> [String]
    filePaths tags = let
        indexes   = fromMaybe [] $ findTag "DirIndexes" tags >>= \t -> tagValue t :: Maybe [Word32]
        dirnames  = fromMaybe [] $ findTag "DirNames" tags >>= \t -> tagValue t :: Maybe [String]
        basenames = fromMaybe [] $ findTag "BaseNames" tags >>= \t -> tagValue t :: Maybe [String]
     in
        zipWith (</>) (map (\i -> dirnames !! fromIntegral i) indexes) basenames

    zipFiles :: [Tag] -> [(String, String, Int, String, String, Int, Maybe String)]
    zipFiles tags = let
        strToMaybe s = if s == "" then Nothing else Just s

        maybeToList (Just l) = l
        maybeToList _        = []

        paths   = filePaths tags
        digests = maybeToList $ findTag "FileMD5s" tags      >>= \t -> tagValue t :: Maybe [String]
        modes   = maybeToList $ findTag "FileModes" tags     >>= \t -> (tagValue t :: Maybe [Word16]) >>= Just . map fromIntegral
        users   = maybeToList $ findTag "FileUserName" tags  >>= \t -> tagValue t :: Maybe [String]
        groups  = maybeToList $ findTag "FileGroupName" tags >>= \t -> tagValue t :: Maybe [String]
        mtimes  = maybeToList $ findTag "FileMTimes" tags    >>= \t -> (tagValue t :: Maybe [Word32]) >>= Just . map fromIntegral
        targets = maybeToList $ findTag "FileLinkTos" tags   >>= \t -> (tagValue t :: Maybe [String]) >>= Just . map strToMaybe
     in
        if not (null paths)
        then zip7 paths digests modes users groups mtimes targets
        else []

associateFilesWithBuild :: MonadIO m => Key Builds -> [Key Files] -> SqlPersistT m [Key BuildFiles]
associateFilesWithBuild build files = do
    let zipped = zip (repeat build) files
    mapM (\(bID, fID) -> insert $ BuildFiles bID fID) zipped

loadRPM :: RPM -> IO ()
loadRPM RPM{..} = runSqlite "test.db" $ do
    projectId <- insertProject tags
    sourceId  <- insertSource tags projectId
    buildId   <- insertBuild tags sourceId
    void $ insertBuildSignature sigs buildId
    filesIds  <- insertFiles tags
    void $ associateFilesWithBuild buildId filesIds
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
                            (\(e :: DBException) -> void $ hPutStrLn stderr ("Error importing RPM: " ++ path))
