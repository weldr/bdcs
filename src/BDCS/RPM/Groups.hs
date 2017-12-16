{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: BDCS.RPM.Groups
-- Copyright: (c) 2016-2017 Red Hat, Inc.
-- License: LGPL
--
-- Maintainer: https://github.com/weldr
-- Stability: alpha
-- Portability: portable
--
-- 'Groups' record support for RPM packages.

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

-- | XXX
addPRCO :: MonadIO m => [Tag] -> Key Groups -> T.Text -> T.Text -> SqlPersistT m ()
addPRCO tags groupId tagBase keyName =
    withPRCO tagBase tags $ \(_, expr) -> let
        -- split out the name part of "name >= version"
        exprBase = T.takeWhile (/= ' ')  expr
      in
        insertGroupKeyValue (TextKey keyName) exprBase (Just expr) groupId

-- | XXX
prcoExpressions :: T.Text -> [Tag] -> [(Word32, T.Text)]
prcoExpressions ty tags = let
    ty'   = T.toTitle ty

    names = map T.pack $ findStringListTag (T.unpack ty' ++ "Name") tags
    flags =              findWord32ListTag (T.unpack ty' ++ "Flags") tags
    vers  = map T.pack $ findStringListTag (T.unpack ty' ++ "Version") tags
 in
    zip flags $ map (\(n, f, v) -> T.stripEnd $ T.concat [n, " ", rpmFlagsToOperator f, " ", v])
        (zip3 names flags vers)

-- | Convert the RPM flags value to a comparison operator Text string
rpmFlagsToOperator :: Word32 -> T.Text
rpmFlagsToOperator f =
    if | f `testBit` 1 && f `testBit` 3 -> "<="
       | f `testBit` 1                  -> "<"
       | f `testBit` 2 && f `testBit` 3 -> ">="
       | f `testBit` 2                  -> ">"
       | f `testBit` 3                  -> "="
       | otherwise                      -> ""

-- | Return the list of contexts to which this requirement applies
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
        whenM ((null <$> get) <&&> return (flags `testBit` 8)) $ do
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

-- | XXX
withPRCO :: Monad m => T.Text -> [Tag] -> ((Word32, T.Text) -> m a) -> m ()
withPRCO ty tags fn =
    mapM_ fn (prcoExpressions ty tags)

-- Ignore the suggestion to not use lambda for creating GroupFiles rows, since
-- the lambda makes it more clear what's actually happening
{-# ANN createGroup ("HLint: ignore Avoid lambda" :: String) #-}
-- | XXX
--   XXX Should this be mkGroup for consistency?
createGroup :: MonadIO m => [Key Files] -> [Tag] -> SqlPersistT m (Key Groups)
createGroup fileIds rpm = do
    -- Get the NEVRA so it can be saved as attributes
    let epoch   = findTag "Epoch" rpm >>= \t -> (tagValue t :: Maybe Word32) >>= Just . T.pack . show
    let name    = maybe "" T.pack (findStringTag "Name" rpm)
    let version = maybe "" T.pack (findStringTag "Version" rpm)
    let release = maybe "" T.pack (findStringTag "Release" rpm)
    let arch    = maybe "" T.pack (findStringTag "Arch" rpm)

    -- Create the groups row
    groupId <- insert $ Groups name "rpm" Nothing

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
