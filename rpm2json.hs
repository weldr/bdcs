{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Conduit(($$), (=$), awaitForever, stdinC)
import           Control.Monad(void)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.Aeson(Value(..), toJSON, ToJSON, object, (.=))
import           Data.Aeson.TH(deriveToJSON, defaultOptions)
import           Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Conduit(Conduit, Consumer, yield)
import           Data.Data
import           Data.Word
import           RPM.Parse(parseRPMC)
import           RPM.Tags(Tag)
import           RPM.Types(RPM(..), Lead, SectionHeader, Header(..))

-- make the RPM types JSON-able from the bottom up
-- only doing the to-JSON instead of from-JSON, to avoid headaches and
-- because from JSON isn't terribly useful.
--
-- first, the easy ones, using template magic
deriveToJSON defaultOptions ''Lead
deriveToJSON defaultOptions ''SectionHeader

-- Tags, wow. What we want to see depends on what's in it. In general, the content is:
--   - nothing at all.
--   - a string. might be something good, show it!
--   - an int. Probably don't care, but no harm in showing it anyway
--   - a list of strings or ints which could well be three miles long. do not show these, holy crap
--   - a bytestring, skip it
-- JSON-ize as { "name" : "WhateverTag", "value" : "maybe a value" }

-- first, some utility functions
tagName :: Tag -> String
tagName t = showConstr $ toConstr t

-- This takes the first constructor parameter of the tag, which is where the data goes, and
-- converts it to a TypeRep
tagType :: Tag -> TypeRep
tagType = gmapQi 0 typeOf

-- Use a cast to pull the first parameter out of the constructor.
tagValue :: (Typeable a) => Tag -> Maybe a
tagValue = gmapQi 0 cast

-- There's probably a better way to do this
-- type needs to be explicit on account of OverloadedStrings
stringType :: TypeRep
stringType = typeOf ("" :: String)

stringListType :: TypeRep
stringListType = typeOf ([] :: [String])

word16ListType :: TypeRep
word16ListType = typeOf ([] :: [Word16])

word32Type :: TypeRep
word32Type = typeOf (0 :: Word32)

word32ListType :: TypeRep
word32ListType = typeOf ([] :: [Word32])

word64Type :: TypeRep
word64Type = typeOf (0 :: Word64)

word64ListType :: TypeRep
word64ListType = typeOf ([] :: [Word64])

tagToJSON :: Tag -> Maybe Value
tagToJSON t
    | tt == stringType     = applyJSON (tagValue t :: Maybe String)
    | tt == word32Type     = applyJSON (tagValue t :: Maybe Word32)
    | tt == word64Type     = applyJSON (tagValue t :: Maybe Word64)
    | tt == stringListType = applyJSON (tagValue t :: Maybe [String])
    | tt == word16ListType = applyJSON (tagValue t :: Maybe [Word16])
    | tt == word32ListType = applyJSON (tagValue t :: Maybe [Word32])
    | tt == word64ListType = applyJSON (tagValue t :: Maybe [Word64])
    | otherwise            = Nothing
    where tt = tagType t

          -- Do not let type inference get a hold of this one, or it'll infer based
          -- on the first case and barf on the rest
          applyJSON :: (Functor f, ToJSON a) => f a -> f Value
          applyJSON = fmap toJSON

instance ToJSON Tag where
    toJSON t = let namePair = "name" .= tagName t
                   value = tagToJSON t

                   -- If we have a value, it should be passed to the object below,
                   -- otherwise use an empty list so the object just gets "name".
                   valueList = case value of
                                Just x  -> [ "value" .= x ]
                                Nothing -> []
               in object (namePair : valueList)

-- for Header, skip the headerStore ByteStream
instance ToJSON Header where
    toJSON hs = object [ "headerSectionHeader" .= toJSON (headerSectionHeader hs),
                         "headerTags"          .= toJSON (headerTags hs) ]

-- for the top-level RPM type, skip rpmArchive
instance ToJSON RPM where
    toJSON rpm = object [ "rpmLead"    .= toJSON (rpmLead rpm),
                          "rpmHeaders" .= toJSON (rpmHeaders rpm) ]

-- conduit to encode RPM into a JSON value. Errors are passed through
encodeC :: Monad m => Conduit RPM m Value
encodeC = awaitForever (yield . toJSON)

-- output sink
consumer :: MonadIO m => Consumer Value m ()
consumer = awaitForever (liftIO . C.putStrLn . encodePretty)

main :: IO ()
main =
    void $ runExceptT $ stdinC $$ parseRPMC =$ encodeC =$ consumer
