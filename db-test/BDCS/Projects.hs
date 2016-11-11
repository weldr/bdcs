{-# LANGUAGE LambdaCase #-}

module BDCS.Projects(findProject,
                     insertProject,
                     mkProject)
 where

import Control.Monad((>=>))
import Control.Monad.IO.Class(MonadIO)
import Data.ByteString.Char8(unpack)
import Data.List(elemIndices)
import Data.Maybe(fromJust, listToMaybe)
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
            Nothing   -> insert $ mkProject (fromJust projectName) rpm `throwIfNothing` DBException "Couldn't make Projects record"
            Just proj -> return proj
 where
    -- the closest to a project name we have is the srpm name, e.g., pykickstart-2.32-1.fc26.src.rpm
    -- This is essentially N-V-R.A.rpm. rpm does not allow hyphens in version of release, and epoch is
    -- not included in the SRPM name, so we can just take everything before the second-to-last hyphen
    -- as the name.
    srpm = findStringTag "SourceRPM" rpm

    srpmToName :: String -> String
    srpmToName s = 
        -- Find all the hyphens and take the second to last result
        let nameHyphenIndex = head $ tail $ reverse $ elemIndices '-' s
        in fst $ splitAt nameHyphenIndex s
    
    -- apply the function to the Maybe from findStringTag
    projectName = fmap srpmToName srpm

            
mkProject :: String -> [Tag] -> Maybe Projects
mkProject projectName tags = do
    let name     = projectName
    summary     <- findByteStringTag "Summary" tags >>= Just . unpack
    description <- findByteStringTag "Description" tags >>= Just . unpack
    let homepage = findStringTag "URL" tags

    -- FIXME:  Where to get this from?
    let upstream_vcs = "UPSTREAM_VCS"

    return $ Projects name summary description homepage upstream_vcs
