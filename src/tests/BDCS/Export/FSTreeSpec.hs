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

    it "Symlinks moving placeholders to symlinks" $
        let dir_a = directoryTemplate{filesPath="/a/b/c"}
            dir_b = directoryTemplate{filesPath="/x/b"}
            link_d = symlinkTemplate{filesPath="/d", filesTarget = Just "/x"}
            link_a = symlinkTemplate{filesPath="/a", filesTarget = Just "/d"}
        in addFiles [dir_a, dir_b, link_d, link_a] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Just link_a) [],
                    Node ("x", Nothing) [
                        Node ("b", Just dir_b) [
                            Node ("c", Just dir_a) []
                        ]
                    ],
                    Node ("d", Just link_d) []
                ]
            ])
        )

    it "Symlinks moving placeholders to replace symlinks" $
        let dir_a = directoryTemplate{filesPath="/a/b/c"}
            link_b = symlinkTemplate{filesPath="/x/b", filesTarget = Just "/e"}
            link_a = symlinkTemplate{filesPath="/a", filesTarget = Just "/x"}
        in addFiles [dir_a, link_b, link_a] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Just link_a) [],
                    Node ("x", Nothing) [
                        Node ("b", Just link_b) []
                    ],
                    Node ("e", Nothing) [
                        Node ("c", Just dir_a) []
                    ]
                ]
            ])
        )

    it "Symlinks moving placeholders to replace symlinks, but with another symlink in the way" $
        let dir_a = directoryTemplate{filesPath="/a/b/c"}
            link_b = symlinkTemplate{filesPath="/x/b", filesTarget = Just "/e"}
            link_e = symlinkTemplate{filesPath="/e/c", filesTarget = Just "/y"}
            link_a = symlinkTemplate{filesPath="/a", filesTarget = Just "/x"}
        in addFiles [dir_a, link_b, link_e, link_a] >>= (`shouldSatisfy` isLeft)

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

    it "Adding a file twice" $
        let file = regularTemplate{filesPath="/a"}
        in addFiles [file, file] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Just file) []
                ]
            ])
         )

    it "Adding a symlink twice" $
        let symlink = symlinkTemplate{filesPath="/a", filesTarget=Just "regular"}
        in addFiles [symlink, symlink] >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("a", Just symlink) []
                ]
            ])
         )

    it "addFileToTree, replace file with self, replace=False" $
        runExceptT (addFileToTree False existingFileTree existingFile) >>= (`shouldBe` Right existingFileTree)

    it "addFileToTre, replace file with self, replace=True" $
        runExceptT (addFileToTree True existingFileTree existingFile) >>= (`shouldBe` Right existingFileTree)

    it "addFileToTree, replace file with directory, replace=False" $
        runExceptT (addFileToTree False existingFileTree existingDir) >>= (`shouldSatisfy` isLeft)

    it "addFileToTree, replace file with directory, replace=True" $
        runExceptT (addFileToTree True existingFileTree existingDir) >>= (`shouldSatisfy` isLeft)

    it "addFileToTree, replace file with a different file, replace=False" $
        runExceptT (addFileToTree False existingFileTree newFile) >>= (`shouldSatisfy` isLeft)

    it "addFileToTree, replace file with a different file, replace=True" $
        runExceptT (addFileToTree True existingFileTree newFile) >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("existing", Just newFile) []
                ]
            ])
        )

    it "addFileToTree, replace directory with self, replace=False" $
        runExceptT (addFileToTree False existingDirTree existingDir) >>= (`shouldBe` Right existingDirTree)

    it "addFileToTree, replace directory with self, replace=True" $
        runExceptT (addFileToTree True existingDirTree existingDir) >>= (`shouldBe` Right existingDirTree)

    it "addFileToTree, replace directory with file, replace=False" $
        runExceptT (addFileToTree False existingDirTree existingFile) >>= (`shouldSatisfy` isLeft)

    it "addFileToTree, replace directory with file, replace=True" $
        runExceptT (addFileToTree True existingDirTree existingFile) >>= (`shouldSatisfy` isLeft)

    it "addFileToTree, replace directory with different directory, replace=False" $
        runExceptT (addFileToTree False existingDirTree newDir) >>= (`shouldSatisfy` isLeft)

    it "addFileToTree, replace directory with different directory, replace=True" $
        runExceptT (addFileToTree True existingDirTree newDir) >>= (`shouldBe` Right (
            Node ("", Nothing) [
                Node ("/", Nothing) [
                    Node ("existing", Just newDir) [
                        Node ("file", Just regularTemplate{filesPath="/existing/file"}) []
                    ]
                ]
            ])
        )
 where
    -- Add a list of files and return the tree
    addFiles :: Monad m => [Files] -> m (Either String FSTree)
    addFiles files = runExceptT $ runConduit $ CL.sourceList files .| filesToTree

    existingDir :: Files
    existingDir = directoryTemplate{filesPath="/existing"}

    newDir :: Files
    newDir = existingDir{filesMode = fromIntegral $ directoryMode .|. 0o700}

    existingDirTree :: FSTree
    existingDirTree = Node ("", Nothing) [
        Node ("/", Nothing) [
            Node ("existing", Just existingDir) [
                Node ("file", Just regularTemplate{filesPath="/existing/file"}) []
            ]
        ]
     ]

    existingFile :: Files
    existingFile = regularTemplate{filesPath="/existing", filesCs_object=Just "abcd"}

    newFile :: Files
    newFile = existingFile{filesCs_object=Just "defg"}

    existingFileTree :: FSTree
    existingFileTree = Node ("", Nothing) [
        Node ("/", Nothing) [
            Node ("existing", Just existingFile) []
        ]
     ]

    -- Just fill in the paths
    regularTemplate :: Files
    regularTemplate = Files "" "root" "root" 0 Nothing (fromIntegral $ regularFileMode .|. 0o644) 0 Nothing

    directoryTemplate :: Files
    directoryTemplate = Files "" "root" "root" 0 Nothing (fromIntegral $ directoryMode .|. 0o755) 0 Nothing

    symlinkTemplate :: Files
    symlinkTemplate = Files "" "root" "root" 0 Nothing (fromIntegral $ symbolicLinkMode .|. 0o644) 0 Nothing
