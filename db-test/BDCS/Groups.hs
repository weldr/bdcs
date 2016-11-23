{-# LANGUAGE LambdaCase #-}

module BDCS.Groups(createGroup,
                   findRequires)
 where

import Control.Monad(forM_, void)
import Control.Monad.IO.Class(MonadIO)
import Data.Maybe(fromMaybe, listToMaybe)
import Database.Esqueleto

import           BDCS.DB
import           BDCS.KeyValue(findKeyValue, insertKeyValue)
import qualified BDCS.ReqType as RT
import           RPM.Tags(Tag, findStringTag, findStringListTag)

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
    forM_ [("name", name), ("version", version), ("release", release), ("arch", arch)] $ \(k, v) ->
        findKeyValue k v >>= \case
            Nothing -> insertKeyValue k v >>= \kvId -> insert $ GroupKeyValues groupId kvId
            Just kv -> insert $ GroupKeyValues groupId kv

    -- Create the Provides attributes
    -- TODO versions, flags
    mapM_ (\provide -> findKeyValue "rpm-provide" provide >>= \case
                           Nothing -> insertKeyValue "rpm-provide" provide >>= \kvId -> insert $ GroupKeyValues groupId kvId
                           Just kv -> insert $ GroupKeyValues groupId kv)
          (findStringListTag "ProvideName" tags)

    -- Create the requirements
    -- TODO versions, flags
    forM_ (findStringListTag "RequireName" tags) $ \reqName -> do
        reqId <- findRequires RT.RPM RT.Runtime RT.Must reqName >>= \case
                     Nothing  -> insert $ Requirements RT.RPM RT.Runtime RT.Must reqName
                     Just rid -> return rid

        void $ insert $ GroupRequirements groupId reqId

    return groupId

findRequires :: MonadIO m => RT.ReqLanguage -> RT.ReqContext -> RT.ReqStrength -> String -> SqlPersistT m (Maybe (Key Requirements))
findRequires reqLang reqCtx reqStrength reqName = do
    ndx <- select $ from $ \r -> do
           where_ (r ^. RequirementsReq_language ==. val reqLang &&.
                   r ^. RequirementsReq_context ==. val reqCtx &&.
                   r ^. RequirementsReq_strength ==. val reqStrength &&.
                   r ^. RequirementsReq_expr ==. val reqName)
           limit 1
           return (r ^. RequirementsId)
    return $ listToMaybe (map unValue ndx)
