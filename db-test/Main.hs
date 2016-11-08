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
import           Database.Esqueleto
import           Database.Persist.Sqlite(runSqlite)
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           System.IO(hPutStrLn, stderr)

import BDCS.Builds(associateBuildWithPackage, insertBuild)
import BDCS.DB
import BDCS.Exceptions
import BDCS.Files(associateFilesWithBuild, associateFilesWithPackage, insertFiles)
import BDCS.Groups(createGroup)
import BDCS.Packages(insertPackageName)
import BDCS.Projects(insertProject)
import BDCS.Sources(insertSource)
import RPM.Parse(parseRPMC)
import RPM.Tags
import RPM.Types

--
-- BUILDS
--

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
