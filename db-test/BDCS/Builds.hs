{-# LANGUAGE LambdaCase #-}

module BDCS.Builds(associateBuildWithPackage,
                   findBuild,
                   insertBuild,
                   mkBuild)
 where

import Control.Monad.IO.Class(MonadIO)
import Data.ByteString.Char8(pack)
import Data.Maybe(listToMaybe)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import Data.Word(Word32)
import Database.Esqueleto

import BDCS.DB
import BDCS.Exceptions(DBException(..), throwIfNothing, throwIfNothingOtherwise)
import RPM.Tags(Tag, findStringTag, findTag, tagValue)

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
            Nothing  -> insert $ mkBuild rpm sourceId `throwIfNothing` DBException "Couldn't make Builds record"
            Just bld -> return bld

mkBuild :: [Tag] -> Key Sources -> Maybe Builds
mkBuild tags sourceId = do
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

associateBuildWithPackage :: MonadIO m => Key Builds -> Key KeyVal -> SqlPersistT m (Key BuildKeyValues)
associateBuildWithPackage buildId kvId =
    insert $ BuildKeyValues buildId kvId
