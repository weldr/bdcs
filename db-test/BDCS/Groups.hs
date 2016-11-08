module BDCS.Groups(createGroup)
 where

import Control.Monad(void)
import Control.Monad.IO.Class(MonadIO)
import Data.Maybe(fromMaybe)
import Database.Esqueleto

import           BDCS.DB
import           BDCS.KeyValue(insertKeyValue)
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
