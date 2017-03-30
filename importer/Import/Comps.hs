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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Import.Comps(CompsPkg(..),
                    CompsGroup(..),
                    loadFromFile,
                    loadFromURL)
 where

import           Control.Monad.Reader(ReaderT)
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit((.|), runConduitRes)
import qualified Data.Text as T
import           Network.HTTP.Conduit(path)
import           Network.HTTP.Simple(Request)
import           Text.XML(Document, sinkDoc)
import           Text.XML.Cursor
import           Text.XML.Stream.Parse(def)

import Import.Conduit(getFromFile, getFromURL, ungzipIfCompressed)
import Import.State(ImportState)

data CompsPkg = CPMandatory T.Text
              | CPDefault T.Text
              | CPOptional T.Text
              | CPUnknown T.Text
              | T.Text `CPRequires` T.Text
 deriving(Show)

data CompsGroup = CompsGroup T.Text T.Text [CompsPkg]
 deriving(Show)

parseCompsPkg :: Cursor -> [CompsPkg]
parseCompsPkg cursor = do
    let tys   = cursor $| attribute "type"
    let names = cursor $/ content
    let reqs  = cursor $| attribute' "requires"

    map toCompsPkg (zip3 tys names reqs)
 where
    -- Like attribute, but returns an empty string instead of an empty list.  This is so tys,
    -- names, and reqs above are all the same length and the tuples line up.
    attribute' n c = case attribute n c of
        [] -> [""]
        x  -> x

    toCompsPkg (t, n, r) = case t of
        "mandatory"     -> CPMandatory n
        "default"       -> CPDefault n
        "optional"      -> CPOptional n
        "conditional"   -> if r == "" then CPUnknown n else n `CPRequires` r
        -- Shouldn't happen, but this marks them so we'll know there's something to look into.
        _               -> CPUnknown n

parseCompsGroup :: Cursor -> CompsGroup
parseCompsGroup cursor = do
    let groupIds    = cursor $/ laxElement "id"   &/ content
    let groupNames  = cursor $/ laxElement "name" &/ content
    let packages    = cursor $// laxElement "packagereq" >=> parseCompsPkg
    CompsGroup (head groupIds) (head groupNames) packages

extractGroups :: Document -> [CompsGroup]
extractGroups doc = let
    cursor = fromDocument doc
    groupCursors = cursor $// laxElement "group"
 in
    map parseCompsGroup groupCursors

loadFromURL :: Request -> ReaderT ImportState IO ()
loadFromURL metadataRequest = do
    groups <- extractGroups <$> runConduitRes (readMetadataPipeline metadataRequest)
    -- FIXME:  For now we don't actually do any loading.  There's a lot of questions
    -- about how this is going to fit into the database, but I want to make sure this
    -- code doesn't get lost.
    return ()
 where
    readMetadataPipeline request = getFromURL request .| ungzipIfCompressed (C8.unpack $ path request) .| sinkDoc def

loadFromFile :: String -> ReaderT ImportState IO ()
loadFromFile metadataPath = do
    groups <- extractGroups <$> runConduitRes (readMetadataPipeline metadataPath)
    -- FIXME:  For now we don't actually do any loading.  There's a lot of questions
    -- about how this is going to fit into the database, but I want to make sure this
    -- code doesn't get lost.
    return ()
 where
    readMetadataPipeline p = getFromFile p .| ungzipIfCompressed p .| sinkDoc def
