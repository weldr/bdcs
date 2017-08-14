-- Copyright (C) 2016-2017 Red Hat, Inc.
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

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module BDCS.RPM.Groups(createGroup)
 where

import           Codec.RPM.Tags(Tag, findStringTag, findStringListTag, findTag, findWord32ListTag, tagValue)
import           Control.Conditional((<&&>), whenM)
import           Control.Monad(forM_, void, when)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State(State, execState, get, modify)
import           Data.Bits(testBit)
import           Data.Maybe(fromJust, isJust)
import qualified Data.Text as T
import           Data.Word(Word32)
import           Database.Persist.Sql(SqlPersistT, insert)

import           BDCS.DB
import           BDCS.GroupKeyValue(insertGroupKeyValue)
import           BDCS.KeyType
import           BDCS.Requirements(insertGroupRequirement, insertRequirement)
import qualified BDCS.ReqType as RT
import           BDCS.RPM.Requirements(mkGroupRequirement, mkRequirement)

addPRCO :: MonadIO m => [Tag] -> Key Groups -> T.Text -> T.Text -> SqlPersistT m ()
addPRCO tags groupId tagBase keyName =
    withPRCO tagBase tags $ \(_, expr) -> let
        -- split out the name part of "name >= version"
        exprBase = T.takeWhile (/= ' ')  expr
      in
        insertGroupKeyValue (TextKey keyName) exprBase (Just expr) groupId

prcoExpressions :: T.Text -> [Tag] -> [(Word32, T.Text)]
prcoExpressions ty tags = let
    ty'   = T.toTitle ty

    names = map T.pack $ findStringListTag (T.unpack ty' ++ "Name") tags
    flags =              findWord32ListTag (T.unpack ty' ++ "Flags") tags
    vers  = map T.pack $ findStringListTag (T.unpack ty' ++ "Version") tags
 in
    zip flags $ map (\(n, f, v) -> T.stripEnd $ T.concat [n, " ", rpmFlagsToOperator f, " ", v])
        (zip3 names flags vers)

rpmFlagsToOperator :: Word32 -> T.Text
rpmFlagsToOperator f =
    if | f `testBit` 1 && f `testBit` 3 -> "<="
       | f `testBit` 1                  -> "<"
       | f `testBit` 2 && f `testBit` 3 -> ">="
       | f `testBit` 2                  -> ">"
       | f `testBit` 3                  -> "="
       | otherwise                      -> ""

-- Return the list of contexts to which this requirement applies
-- RPM interprets a combination of RPMSENSE_SCRIPT_* flags as meaning that the requirement is needed for
-- each of those script types. If the requirement is *also* needed for Runtime, it will appear
-- again in the requirements without any SCRIPT_* flags.
--
-- RPMSENSE_INTERP is annoying: it doesn't add any information (INTERP | SCRIPT_PRE is just another %pre requirement)
-- *unless* it appears on its own, which instead means that it applies to all script types present.
--
-- Ignoring RPMSENSE_CONFIG, since config(whatever) requirements have matching config(whatever) provides without
-- bringing flags into it.
--
-- Also ignoring RPMSENSE_TRIGGER*, since they don't appear to ever be used
rpmFlagsToContexts :: [Tag] -> Word32 -> [RT.ReqContext]
rpmFlagsToContexts tags flags =
    execState rpmFlagsToContextsState []
 where
    rpmFlagsToContextsState :: State [RT.ReqContext] ()
    rpmFlagsToContextsState = do
        when (flags `testBit`  9) (modify (RT.ScriptPre:))
        when (flags `testBit` 10) (modify (RT.ScriptPost:))
        when (flags `testBit` 11) (modify (RT.ScriptPreUn:))
        when (flags `testBit` 12) (modify (RT.ScriptPostUn:))
        when (flags `testBit`  7) (modify (RT.ScriptPreTrans:))
        when (flags `testBit`  5) (modify (RT.ScriptPostTrans:))
        when (flags `testBit` 13) (modify (RT.ScriptVerify:))

        -- Check for a bare RPMSENSE_INTERP
        whenM ((null <$> get) <&&> (return $ flags `testBit` 8)) $ do
            when ((isJust . findTag "PreIn")  tags) (modify (RT.ScriptPre:))
            when ((isJust . findTag "PostIn") tags) (modify (RT.ScriptPost:))
            when ((isJust . findTag "PreUn")  tags) (modify (RT.ScriptPreUn:))
            when ((isJust . findTag "PostUn") tags) (modify (RT.ScriptPost:))
            when ((isJust . findTag "PreTrans") tags)  (modify (RT.ScriptPreTrans:))
            when ((isJust . findTag "PostTrans") tags) (modify (RT.ScriptPostTrans:))
            when ((isJust . findTag "VerifyScript") tags) (modify (RT.ScriptVerify:))

        when (flags `testBit` 24) (modify (RT.Feature:))

        -- If nothing else set, return Runtime
        whenM (null <$> get) (modify (RT.Runtime:))

withPRCO :: Monad m => T.Text -> [Tag] -> ((Word32, T.Text) -> m a) -> m ()
withPRCO ty tags fn =
    mapM_ fn (prcoExpressions ty tags)

-- Ignore the suggestion to not use lambda for creating GroupFiles rows, since
-- the lambda makes it more clear what's actually happening
{-# ANN createGroup ("HLint: ignore Avoid lambda" :: String) #-}
createGroup :: MonadIO m => [Key Files] -> [Tag] -> SqlPersistT m (Key Groups)
createGroup fileIds rpm = do
    -- Get the NEVRA so it can be saved as attributes
    let epoch   = findTag "Epoch" rpm >>= \t -> (tagValue t :: Maybe Word32) >>= Just . T.pack . show
    let name    = maybe "" T.pack (findStringTag "Name" rpm)
    let version = maybe "" T.pack (findStringTag "Version" rpm)
    let release = maybe "" T.pack (findStringTag "Release" rpm)
    let arch    = maybe "" T.pack (findStringTag "Arch" rpm)

    -- Create the groups row
    groupId <- insert $ Groups name "rpm"

    -- Create the group_files rows
    mapM_ (\fId -> insert $ GroupFiles groupId fId) fileIds

    -- Create the (E)NVRA attributes
    -- FIXME could at least deduplicate name and arch real easy
    forM_ [(TextKey "name", name), (TextKey "version", version), (TextKey "release", release), (TextKey "arch", arch)] $ \tup ->
        uncurry insertGroupKeyValue tup Nothing groupId

    -- Add the epoch attribute, when it exists.
    when (isJust epoch) $ void $
        insertGroupKeyValue (TextKey "epoch") (fromJust epoch) Nothing groupId

    forM_ [("Provide", "rpm-provide"), ("Conflict", "rpm-conflict"), ("Obsolete", "rpm-obsolete"), ("Order", "rpm-install-after")] $ \tup ->
        uncurry (addPRCO rpm groupId) tup

    -- Create the Requires attributes
    forM_ [("Require", RT.Must), ("Recommend", RT.Should), ("Suggest", RT.May),
           ("Supplement", RT.ShouldIfInstalled), ("Enhance", RT.MayIfInstalled)] $ \tup ->
        withPRCO (fst tup) rpm $ \(flags, expr) ->
            forM_ (rpmFlagsToContexts rpm flags) $ \context -> do
                reqId <- insertRequirement $ mkRequirement context (snd tup) expr

                -- Don't insert a requirement for a group more than once.  RPMs can have the same
                -- requirement listed multiple times, for whatever reason, but we want to reduce
                -- duplication.
                insertGroupRequirement $ mkGroupRequirement groupId reqId

    return groupId
