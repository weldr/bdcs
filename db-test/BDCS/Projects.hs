{-# LANGUAGE LambdaCase #-}

module BDCS.Projects(findProject,
                     insertProject,
                     mkProject)
 where

import Control.Monad((>=>))
import Control.Monad.IO.Class(MonadIO)
import Data.ByteString.Char8(unpack)
import Data.Maybe(listToMaybe)
import Database.Esqueleto

import BDCS.DB
import BDCS.Exceptions(DBException(..), throwIfNothing, throwIfNothingOtherwise)
import RPM.Tags(Tag, findByteStringTag, findStringTag)

findProject :: MonadIO m => String -> SqlPersistT m (Maybe (Key Projects))
findProject name = do
    ndx <- select $ from $ \proj -> do
           where_ (proj ^. ProjectsName ==. val name)
           limit 1
           return (proj ^. ProjectsId)
    return $ listToMaybe (map unValue ndx)

insertProject :: MonadIO m => [Tag] -> SqlPersistT m (Key Projects)
insertProject rpm =
    throwIfNothingOtherwise projectName (DBException "No SourceRPM tag") $
        findProject >=> \case
            Nothing   -> insert $ mkProject rpm `throwIfNothing` DBException "Couldn't make Projects record"
            Just proj -> return proj
 where
    -- FIXME:  SourceRPM looks like "pykickstart-2.32-1.fc26.src.rpm", but we really just want "pykickstart"
    -- here.  I'll get to that.
    projectName = findStringTag "SourceRPM" rpm

mkProject :: [Tag] -> Maybe Projects
mkProject tags = do
    name        <- projectName
    summary     <- findByteStringTag "Summary" tags >>= Just . unpack
    description <- findByteStringTag "Description" tags >>= Just . unpack
    let homepage = findStringTag "URL" tags

    -- FIXME:  Where to get this from?
    let upstream_vcs = "UPSTREAM_VCS"

    return $ Projects name summary description homepage upstream_vcs
 where
    -- FIXME:  SourceRPM looks like "pykickstart-2.32-1.fc26.src.rpm", but we really just want "pykickstart"
    -- here.  I'll get to that.
    projectName = findStringTag "SourceRPM" tags
