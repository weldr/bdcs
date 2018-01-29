-- Copyright (C) 2017 Red Hat, Inc.
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

{-# LANGUAGE OverloadedStrings #-}

module BDCS.Export.FSTreeSpec(spec)
 where

import BDCS.DB(Files(..))
import BDCS.Export.FSTree

import           Control.Monad.Except(runExceptT)
import           Data.Bits((.|.))
import           Data.Conduit((.|), runConduit)
import qualified Data.Conduit.List as CL
import           Data.Either(isLeft)
import           Data.Tree(Tree(..))
import           System.Posix.Files(directoryMode, regularFileMode, symbolicLinkMode)

import Test.Hspec

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
spec :: Spec
spec = describe "Export.FSTree Tests" $ do
    it "Adding a regular file to an empty tree" $
        let file_a = regularTemplate{filesPath="/a"}
        in addFiles [file_a] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Just file_a) []
                ]
            ])
        )

    it "Adding directories before children" $
        let dir_slash = directoryTemplate{filesPath="/"}
            dir_a     = directoryTemplate{filesPath="/a"}
            file_b    = regularTemplate{filesPath="/a/b"}
        in addFiles [dir_slash, dir_a, file_b] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Just dir_slash) [
                    Node ("a", Just dir_a) [
                        Node ("b", Just file_b) []
                    ]
                ]
            ])
        )

    it "Adding directories after children" $
        let dir_slash = directoryTemplate{filesPath="/"}
            dir_a     = directoryTemplate{filesPath="/a"}
            file_b    = regularTemplate{filesPath="/a/b"}
        in addFiles [file_b, dir_a, dir_slash] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Just dir_slash) [
                    Node ("a", Just dir_a) [
                        Node ("b", Just file_b) []
                    ]
                ]
            ])
        )

    it "Replace a directory with a symlink" $
        let bin_grep = regularTemplate{filesPath="/bin/grep"}
            bin_link = symlinkTemplate{filesPath="/bin", filesTarget=Just "/usr/bin"}
        in addFiles [bin_grep, bin_link] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("bin", Just bin_link) [],
                    Node ("usr", Nothing) [
                        Node ("bin", Nothing) [
                            Node ("grep", Just bin_grep) []
                        ]
                    ]
                ]
            ])
        )

    it "Add a file to a symlink path" $
        let bin_grep = regularTemplate{filesPath="/bin/grep"}
            bin_link = symlinkTemplate{filesPath="/bin", filesTarget=Just "/usr/bin"}
        in addFiles [bin_link, bin_grep] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("bin", Just bin_link) [],
                    Node ("usr", Nothing) [
                        Node ("bin", Nothing) [
                            Node ("grep", Just bin_grep) []
                        ]
                    ]
                ]
            ])
        )

    it "Symlinks moving placeholders" $
        let dir_a = directoryTemplate{filesPath="/a/b/c"}
            dir_d = directoryTemplate{filesPath="/d/b"}
            link  = symlinkTemplate{filesPath="/a", filesTarget = Just "d"}
        in addFiles [dir_a, dir_d, link] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Just link) [],
                    Node ("d", Nothing) [
                        Node ("b", Just dir_d) [
                            Node ("c", Just dir_a) []
                        ]
                    ]
                ]
            ])
        )

    it "Symlink loop" $
        let link_a = symlinkTemplate{filesPath="/a", filesTarget=Just "b"}
            link_b = symlinkTemplate{filesPath="/b", filesTarget=Just "a"}
            file   = regularTemplate{filesPath="/b/c"}
        in addFiles [link_a, link_b, file] >>= (`shouldSatisfy` isLeft)

    it "Symlink with .." $
        let link = symlinkTemplate{filesPath="/a/b", filesTarget=Just "../d"}
            file = regularTemplate{filesPath="/a/b/c"}
        in addFiles [link, file] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Nothing) [
                        Node ("b", Just link) []
                    ],
                    Node ("d", Nothing) [
                        Node ("c", Just file) []
                    ]
                ]
            ])
        )

    it "Symlink with ." $
        let link = symlinkTemplate{filesPath="/a/b", filesTarget=Just "./d"}
            file = regularTemplate{filesPath="/a/b/c"}
        in addFiles [link, file] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Nothing) [
                        Node ("b", Just link) [],
                        Node ("d", Nothing) [
                            Node ("c", Just file) []
                        ]
                    ]
                ]
            ])
        )

    it "Symlink loop, different case" $
        let link = symlinkTemplate{filesPath="/a", filesTarget=Just "."}
            file = regularTemplate{filesPath="/a"}
        in addFiles [link, file] >>= (`shouldSatisfy` isLeft)
 where
    -- Add a list of files and return the tree
    addFiles :: Monad m => [Files] -> m (Either String FSTree)
    addFiles files = runExceptT $ runConduit $ CL.sourceList files .| filesToTree

    -- Just fill in the paths
    regularTemplate :: Files
    regularTemplate = Files "" "root" "root" 0 Nothing (fromIntegral $ regularFileMode .|. 0o644) 0 Nothing

    directoryTemplate :: Files
    directoryTemplate = Files "" "root" "root" 0 Nothing (fromIntegral $ directoryMode .|. 0o755) 0 Nothing

    symlinkTemplate :: Files
    symlinkTemplate = Files "" "root" "root" 0 Nothing (fromIntegral $ symbolicLinkMode .|. 0o644) 0 Nothing
