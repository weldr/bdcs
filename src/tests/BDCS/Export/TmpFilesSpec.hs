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

module BDCS.Export.TmpFilesSpec(spec)
  where

import BDCS.Export.TmpFiles

import Test.Hspec

spec :: Spec
spec = describe "Export.TmpFiles Tests" $ do
    it "New Directory" $
        parseConfString "d /usr/sbin 0740 root root -\n"
        `shouldBe`
        Right [TmpFileEntry{tfeType=NewDirectory, tfePath="/usr/sbin", tfeMode=Just 0o740, tfeUid=Just "root", tfeGid=Just "root", tfeAge=Nothing, tfeArg=Nothing}]

    it "Modify Directory" $
        parseConfString "e /etc/pki/secrets 0700 pki pki -\n"
        `shouldBe`
        Right [TmpFileEntry{tfeType=ModifyDirectory, tfePath="/etc/pki/secrets", tfeMode=Just 0o700, tfeUid=Just "pki", tfeGid=Just "pki", tfeAge=Nothing, tfeArg=Nothing}]

    it "New File, no content" $
        parseConfString "f /etc/bdcs.conf 0700 weldr weldr -\n"
        `shouldBe`
         Right [TmpFileEntry{tfeType=NewFile, tfePath="/etc/bdcs.conf", tfeMode=Just 0o700, tfeUid=Just "weldr", tfeGid=Just "weldr", tfeAge=Nothing, tfeArg=Nothing}]

    it "New File, with content" $
        parseConfString "f /etc/weldr.conf - weldr weldr - I am the content\n"
        `shouldBe`
        Right [TmpFileEntry{tfeType=NewFile, tfePath="/etc/weldr.conf", tfeMode=Nothing, tfeUid=Just "weldr", tfeGid=Just "weldr", tfeAge=Nothing, tfeArg=Just "I am the content"}]

    it "New Symlink" $
        parseConfString "L /var/run - - - - ../run\n"
        `shouldBe`
        Right [TmpFileEntry{tfeType=NewSymlink, tfePath="/var/run", tfeMode=Nothing, tfeUid=Nothing, tfeGid=Nothing, tfeAge=Nothing, tfeArg=Just "../run"}]

    it "Replace Symlink" $
        parseConfString "L+ /var/lock - - - - ../run/lock\n"
        `shouldBe`
        Right [TmpFileEntry{tfeType=ReplaceSymlink, tfePath="/var/lock", tfeMode=Nothing, tfeUid=Nothing, tfeGid=Nothing, tfeAge=Nothing, tfeArg=Just "../run/lock"}]

    it "Parse multiple entries" $
        parseConfString "L+ /var/lock - - - - ../run/lock\nL /var/run - - - - ../run\nf /etc/bdcs.conf 0700 weldr weldr -\n"
        `shouldBe`
        Right [
            TmpFileEntry{tfeType=ReplaceSymlink, tfePath="/var/lock", tfeMode=Nothing, tfeUid=Nothing, tfeGid=Nothing, tfeAge=Nothing, tfeArg=Just "../run/lock"},
            TmpFileEntry{tfeType=NewSymlink, tfePath="/var/run", tfeMode=Nothing, tfeUid=Nothing, tfeGid=Nothing, tfeAge=Nothing, tfeArg=Just "../run"},
            TmpFileEntry{tfeType=NewFile, tfePath="/etc/bdcs.conf", tfeMode=Just 0o700, tfeUid=Just "weldr", tfeGid=Just "weldr", tfeAge=Nothing, tfeArg=Nothing}
        ]

