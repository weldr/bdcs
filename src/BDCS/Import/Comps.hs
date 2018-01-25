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

module BDCS.Import.Comps(CompsPkg(..),
                         CompsGroup(..),
                         loadFromURI)
 where

import           Control.Monad.Reader(ReaderT)
import           Data.Conduit((.|), runConduitRes)
import           Data.Maybe(mapMaybe)
import qualified Data.Text as T
import           Network.URI(URI(..))
import           Text.XML(Document, sinkDoc)
import           Text.XML.Cursor
import           Text.XML.Stream.Parse(def)

import BDCS.Import.Conduit(getFromURI, ungzipIfCompressed)
import BDCS.Import.State(ImportState)

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

    toCompsPkg ("mandatory", n, _)    = CPMandatory n
    toCompsPkg ("default", n, _)      = CPDefault n
    toCompsPkg ("optional", n, _)     = CPOptional n
    toCompsPkg ("conditional", n, "") = CPUnknown n
    toCompsPkg ("conditional", n, r)  = n `CPRequires` r
    -- Shouldn't happen, but this marks them so we'll know there's something to look into.
    toCompsPkg (_, n, _)              = CPUnknown n

parseCompsGroup :: Cursor -> Maybe CompsGroup
parseCompsGroup cursor = let
    groupIds    = cursor $/ laxElement "id"   &/ content
    groupNames  = cursor $/ laxElement "name" &/ content
    packages    = cursor $// laxElement "packagereq" >=> parseCompsPkg
 in
    case (groupIds, groupNames) of
        (fstId:_, fstName:_) -> Just $ CompsGroup fstId fstName packages
        _                    -> Nothing

extractGroups :: Document -> [CompsGroup]
extractGroups doc = let
    cursor = fromDocument doc
    groupCursors = cursor $// laxElement "group"
 in
    mapMaybe parseCompsGroup groupCursors

loadFromURI :: URI -> ReaderT ImportState IO ()
loadFromURI uri = do
    _groups <- extractGroups <$> runConduitRes (readMetadataPipeline uri)
    -- FIXME:  For now we don't actually do any loading.  There's a lot of questions
    -- about how this is going to fit into the database, but I want to make sure this
    -- code doesn't get lost.
    return ()
 where
    readMetadataPipeline p = getFromURI p .| ungzipIfCompressed .| sinkDoc def
