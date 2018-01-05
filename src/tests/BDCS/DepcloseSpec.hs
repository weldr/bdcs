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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module BDCS.DepcloseSpec(spec)
 where

import           Control.Monad(void, when)
import           Control.Monad.Except(ExceptT, runExceptT)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Logger(NoLoggingT)
import           Control.Monad.Trans.Resource(MonadBaseControl, ResourceT)
import           Data.Monoid((<>))
import qualified Data.Text as T
import           Database.Persist(insert)
import           Database.Persist.Sql(Key, SqlPersistT, insertKey, toSqlKey)
import           Test.Hspec

import           BDCS.DB(Files(..), GroupFiles(..), Groups(..))
import           BDCS.Depclose(depcloseNEVRAs, depcloseNames)
import           BDCS.Depsolve(Formula(..))
import           BDCS.GroupKeyValue(insertGroupKeyValue)
import           BDCS.KeyType(KeyType(..))
import           BDCS.Requirements(insertGroupRequirement, insertRequirement)
import qualified BDCS.ReqType as RT(ReqContext(..), ReqStrength(..))
import           BDCS.RPM.Requirements(mkGroupRequirement, mkRequirement)
import           Utils(withDb)

-- The solutions provided by depclose are super messy (depsolve is supposed to clean them up),
-- and any changes in depclose are likely to break these tests.
spec :: Spec
spec = describe "BDCS.Depclose Tests" $ do
    let singleton_req = And [Atom (toSqlKey 1)]
    let solution_1 = Right $ And [singleton_req]
    it "depclose, singleton" $
        withDeps (depcloseNEVRAs arches ["singleton-1.0-1.x86_64"]) >>= (`shouldBe` solution_1)

    let simple_req = And [Atom (toSqlKey 2), Or [singleton_req]]
    let solution_2 = Right $ And [simple_req]
    it "depclose, simple NEVRA" $
        withDeps (depcloseNEVRAs arches ["simple-1.0-1.x86_64"]) >>= (`shouldBe` solution_2)

    it "depclose, simple name" $
        withDeps (depcloseNames arches ["simple"]) >>= (`shouldBe` solution_2)

    let simple_chain_req = And [Atom (toSqlKey 3), Or [simple_req]]
    let solution_3 = Right $ And [simple_chain_req]
    it "depclose, simple-chain" $
        withDeps (depcloseNEVRAs arches ["simple-chain-1.0-1.x86_64"]) >>= (`shouldBe` solution_3)

    let provides_file_req = And [Atom (toSqlKey 4)]
    let needs_file_req = And [Atom (toSqlKey 5), Or [provides_file_req]]
    let solution_5 = Right $ And [needs_file_req]
    it "depclose, needs-file" $
        withDeps (depcloseNEVRAs arches ["needs-file-1.0-1.x86_64"]) >>= (`shouldBe` solution_5)

    let conflicts_req = And [Atom (toSqlKey 6), Not (toSqlKey 1)]
    let solution_6 = Right $ And [conflicts_req]
    it "depclose, conflicts" $
        withDeps (depcloseNEVRAs arches ["conflicts-1.0-1.x86_64"]) >>= (`shouldBe` solution_6)

    let obsolete_req = And [Atom (toSqlKey 7), Not (toSqlKey 1)]
    let solution_7 = Right $ And [obsolete_req]
    it "depclose, obsoletes" $
        withDeps (depcloseNEVRAs arches ["obsoletes-1.0-1.x86_64"]) >>= (`shouldBe` solution_7)

    let high_version_req = And [Atom (toSqlKey 10)]
    let need_version_req = And [Atom (toSqlKey 11), Or [high_version_req]]
    let solution_11 = Right $ And [need_version_req]
    it "depclose, versioned requirement" $
        withDeps (depcloseNEVRAs arches ["needs-version-1.0-1.x86_64"]) >>= (`shouldBe` solution_11)

    let obsolete_version_req = And [Atom (toSqlKey 12), Not (toSqlKey 9)]
    let solution_12 = Right $ And [obsolete_version_req]
    it "depclose, versioned obsolete" $
        withDeps (depcloseNEVRAs arches ["obsoletes-version-1.0-1.x86_64"]) >>= (`shouldBe` solution_12)

    let loop_1_req = And [Atom (toSqlKey 14), Or [And [Atom (toSqlKey 15)]]]
    let solution_15 = Right $ And [loop_1_req]
    it "depclose, dependency cycle" $
        withDeps (depcloseNEVRAs arches ["loop-1-1.0-1.x86_64"]) >>= (`shouldBe` solution_15)

    let choice_1_req = And [Atom (toSqlKey 17)]
    let choice_2_req = And [Atom (toSqlKey 18)]
    let choices_req  = And [Atom (toSqlKey 16), Or [choice_1_req, choice_2_req]]
    let solution_16  = Right $ And [choices_req]
    it "depclose, multiple providers" $
        withDeps (depcloseNEVRAs arches ["choices-1.0-1.x86_64"]) >>= (`shouldBe` solution_16)

    let solution_double = Right $ And [needs_file_req, singleton_req]
    it "depclose, two things NEVRAs" $
        withDeps (depcloseNEVRAs arches ["singleton-1.0-1.x86_64", "needs-file-1.0-1.x86_64"]) >>= (`shouldBe` solution_double)

    it "depclose, two things names" $
        withDeps (depcloseNames arches ["singleton", "needs-file"]) >>= (`shouldBe` solution_double)

    it "depclose, no such requirement" $
        withDeps (depcloseNEVRAs arches ["no-such-requirement-1.0-1.x86_64"]) >>= (`shouldBe` Left "Unable to resolve requirement: DepRequirement \"does-not-exist\" Nothing")

    it "depclose, missing provides data" $
        withDeps (depcloseNEVRAs arches ["broken-conflicts-1.0-1.x86_64"]) >>= (`shouldBe` Left "Invalid key/val data")

    it "depclose, no such package" $
        withDeps (depcloseNEVRAs arches ["does-not-exist-1.0-1.x86_64"]) >>= (`shouldBe` Left "No such group does-not-exist-1.0-1.x86_64")

    -- run tests with (mostly) real data
    -- this is more a demonstration of what's wrong with depclose than anything
    let ncurses_base_req = And [Atom (toSqlKey 13)]
    let tzdata_req = And [Atom (toSqlKey 10)]
    let libgcc_req = And [Atom (toSqlKey 7)]

    let libstdcplusplus_req = And [Atom (toSqlKey 8)
                                   -- glibc in parents
                                   -- libgcc in parents
                                  ]

    let xz_libs_req = And [Atom (toSqlKey 5)
                           -- glibc in parents
                          ]

    let pcre_req = And [Atom (toSqlKey 6),
                        -- glibc in parents
                        Or [libstdcplusplus_req],
                        Or [libgcc_req]]

    let libsepol_req = And [Atom (toSqlKey 9)
                            -- glibc in parents
                           ]

    let libselinux_req = And [Atom (toSqlKey 4),
                              -- glibc in parents
                              Or [libsepol_req],
                              Or [pcre_req],
                              Or [xz_libs_req]]

    let glibc_common_req = And [Atom (toSqlKey 3),
                                -- bash in parents
                                -- glibc in parents
                                Or [tzdata_req],
                                Or [libselinux_req]]

    let nss_softokn_freebl_req = And [Atom (toSqlKey 11)
                                      -- bash in parents
                                      -- glibc in parents
                                     ]

    let glibc_req = And [Atom (toSqlKey 2),
                         Or [nss_softokn_freebl_req],
                         Or [glibc_common_req]]

    let ncurses_req = And [Atom (toSqlKey 12),
                           -- glibc in parents
                           -- libgcc in parents
                           -- libstdc++ in parents
                           Or [ncurses_base_req]]

    let bash_req = And [Atom (toSqlKey 1),
                        Or [ncurses_req],
                        Or [glibc_req]]
    let bash_solution = Right $ And [bash_req]
    it "depclose bash NEVRA" $
        withRealDeps (depcloseNEVRAs arches ["bash-4.2.46-12.el7.x86_64"]) >>= (`shouldBe` bash_solution)

    it "depclose bash name" $
        withRealDeps (depcloseNames arches ["bash"]) >>= (`shouldBe` bash_solution)

    let glibc_req_2 = And [Atom (toSqlKey 1000),
                           Or [nss_softokn_freebl_req],
                           Or [glibc_common_req]]
    let bash_req_2 = And [Atom (toSqlKey 1),
                          Or [ncurses_req],
                          Or [glibc_req, glibc_req_2]]
    let bash_solution_2 = Right $ And [bash_req_2]
    it "depclose bash, two glibcs" $
        withGlibcUpgrade (depcloseNEVRAs arches ["bash-4.2.46-12.el7.x86_64"]) >>= (`shouldBe` bash_solution_2)

    -- tar requirements, minus requirements already pulled in by bash (glibc, libselinux)
    let libattr_req = And [Atom (toSqlKey 16)]
    let libacl_req = And [Atom (toSqlKey 15),
                          Or [libattr_req]]
    let tar_with_bash_req = And [Atom (toSqlKey 14),
                                 Or [libacl_req]]
    let tar_with_bash_solution = Right $ And [tar_with_bash_req, bash_req]
    it "depclose bash and tar NEVRAs at the same time" $
        withRealDeps (depcloseNEVRAs arches ["bash-4.2.46-12.el7.x86_64", "tar-2:1.26-29.el7.x86_64"]) >>= (`shouldBe` tar_with_bash_solution)

    it "depclose bash and tar names at the same time" $
        withRealDeps (depcloseNames arches ["bash", "tar"]) >>= (`shouldBe` tar_with_bash_solution)
 where
    arches :: [T.Text]
    arches = ["x86_64"]

    addDeps :: MonadIO m => SqlPersistT m ()
    addDeps = do
        -- singleton, provides itself, requires nothing
        let groupid_1 = toSqlKey 1
        insertNEVRA groupid_1 "singleton" Nothing "1.0" "1" "x86_64"

        -- simple, requires singleton and nothing else
        let groupid_2 = toSqlKey 2
        insertNEVRA groupid_2 "simple" Nothing "1.0" "1" "x86_64"
        addReq groupid_2 "singleton"

        -- simple-chain, requires simple which requires singleton
        let groupid_3 = toSqlKey 3
        insertNEVRA groupid_3 "simple-chain" Nothing "1.0" "1" "x86_64"
        addReq groupid_3 "simple"

        -- provides-file and needs-file, for file-based requirements
        let groupid_4 = toSqlKey 4
        insertNEVRA groupid_4 "provides-file" Nothing "1.0" "1" "x86_64"
        addFile groupid_4 "/what/ever"
        let groupid_5 = toSqlKey 5
        insertNEVRA groupid_5 "needs-file" Nothing "1.0" "1" "x86_64"
        addReq groupid_5 "/what/ever"

        -- conflicts, conflicts with singleton
        let groupid_6 = toSqlKey 6
        insertNEVRA groupid_6 "conflicts" Nothing "1.0" "1" "x86_64"
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "singleton" (Just "singleton") groupid_6

        -- obsoletes, same thing
        let groupid_7 = toSqlKey 7
        insertNEVRA groupid_7 "obsoletes" Nothing "1.0" "1" "x86_64"
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "singleton" (Just "singleton") groupid_7

        -- missing ext_val in a key/val that expects a version
        let groupid_8 = toSqlKey 8
        insertNEVRA groupid_8 "broken-conflicts" Nothing "1.0" "1" "x86_64"
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "missing-ext-value" Nothing groupid_8

        -- versioned requirement
        let groupid_9 = toSqlKey 9
        insertNEVRA groupid_9 "versioned" Nothing "1.0" "1" "x86_64"
        addProvide groupid_9 "version-test" (Just "= 1.0")

        let groupid_10 = toSqlKey 10
        insertNEVRA groupid_10 "versioned" Nothing "2.0" "1" "x86_64"
        addProvide groupid_10 "version-test" (Just "= 2.0")

        let groupid_11 = toSqlKey 11
        insertNEVRA groupid_11 "needs-version" Nothing "1.0" "1" "x86_64"
        addReq groupid_11 "versioned >= 2.0"

        -- obsolete with version
        let groupid_12 = toSqlKey 12
        insertNEVRA groupid_12 "obsoletes-version" Nothing "1.0" "1" "x86_64"
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "versioned" (Just "versioned < 2.0") groupid_12

        -- unsatisfiable, package does not exist
        let groupid_13 = toSqlKey 13
        insertNEVRA groupid_13 "no-such-requirement" Nothing "1.0" "1" "x86_64"
        addReq groupid_13 "does-not-exist"

        -- create a loop
        let groupid_14 = toSqlKey 14
        insertNEVRA groupid_14 "loop-1" Nothing "1.0" "1" "x86_64"
        addReq groupid_14 "loop-2"

        let groupid_15 = toSqlKey 15
        insertNEVRA groupid_15 "loop-2" Nothing "1.0" "1" "x86_64"
        addReq groupid_15 "loop-1"

        -- require something with multiple providers
        let groupid_16 = toSqlKey 16
        insertNEVRA groupid_16 "choices" Nothing "1.0" "1" "x86_64"
        addReq groupid_16 "choices-req"

        let groupid_17 = toSqlKey 17
        insertNEVRA groupid_17 "choice-1" Nothing "1.0" "1" "x86_64"
        addProvide groupid_17 "choices-req" Nothing

        let groupid_18 = toSqlKey 18
        insertNEVRA groupid_18 "choice-2" Nothing "1.0" "1" "x86_64"
        addProvide groupid_18 "choices-req" Nothing

    -- Real dependencies taken from CentOS 7 data
    -- IDs:
    --   1: bash
    --   2: glibc
    --   3: glibc-common
    --   4: libselinux
    --   5: xz-libs
    --   6: pcre
    --   7: libgcc
    --   8: libstdc++
    --   9: libsepol
    --  10: tzdata
    --  11: nss-softokn-freebl
    --  12: ncurses-libs
    --  13: ncurses-base
    --  14: tar
    --  15: libacl
    --  16: libattr
    addRealDeps :: MonadIO m => SqlPersistT m ()
    addRealDeps = do
        -- only the provides that are actually used are included, to try to keep things a little less out of control

        let groupid_1 = toSqlKey 1
        insertNEVRA groupid_1 "bash" Nothing "4.2.46" "12.el7" "x86_64"
        addProvide groupid_1 "/bin/bash" Nothing
        addProvide groupid_1 "/bin/sh" Nothing
        addProvide groupid_1 "config(bash)" (Just "= 4.2.46-12.el7")
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "filesystem" (Just "filesystem < 3") groupid_1
        addFile groupid_1 "/usr/bin/bash"

        -- self-provided
        addReq groupid_1 "/bin/sh"
        addReq groupid_1 "config(bash) = 4.2.46-12.el7"

        -- provided by glibc (id 2)
        addReq groupid_1 "libc.so.6()(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.11)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.15)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_1 "libc.so.6(GLIBC_2.8)(64bit)"
        addReq groupid_1 "libdl.so.2()(64bit)"
        addReq groupid_1 "libdl.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_1 "rtld(GNU_HASH)"

        -- ncurses-libs (id 12)
        addReq groupid_1 "libtinfo.so.5()(64bit)"

        let groupid_2 = toSqlKey 2
        insertNEVRA groupid_2 "glibc" Nothing "2.17" "78.el7" "x86_64"
        addProvide groupid_2 "config(glibc)" (Just "= 2.17-78.el7")
        addProvide groupid_2 "ld-linux-x86-64.so.2()(64bit)" Nothing
        addProvide groupid_2 "ld-linux-x86-64.so.2(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)" Nothing
        addProvide groupid_2 "libCNS.so()(64bit)" Nothing
        addProvide groupid_2 "libGB.so()(64bit)" Nothing
        addProvide groupid_2 "libISOIR165.so()(64bit)" Nothing
        addProvide groupid_2 "libJIS.so()(64bit)" Nothing
        addProvide groupid_2 "libJISX0213.so()(64bit)" Nothing
        addProvide groupid_2 "libKSC.so()(64bit)" Nothing
        addProvide groupid_2 "libc.so.6()(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.10)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.11)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.12)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.13)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.14)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.15)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.17)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.3)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.3.2)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.3.3)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.3.4)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.4)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.6)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.7)(64bit)" Nothing
        addProvide groupid_2 "libc.so.6(GLIBC_2.8)(64bit)" Nothing
        addProvide groupid_2 "libdl.so.2()(64bit)" Nothing
        addProvide groupid_2 "libdl.so.2(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "libm.so.6()(64bit)" Nothing
        addProvide groupid_2 "libm.so.6(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "libnsl.so.1()(64bit)" Nothing
        addProvide groupid_2 "libnsl.so.1(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "libnss_files.so.2()(64bit)" Nothing
        addProvide groupid_2 "libpthread.so.0()(64bit)" Nothing
        addProvide groupid_2 "libpthread.so.0(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "libpthread.so.0(GLIBC_2.3.2)(64bit)" Nothing
        addProvide groupid_2 "libpthread.so.0(GLIBC_2.3.3)(64bit)" Nothing
        addProvide groupid_2 "libresolv.so.2()(64bit)" Nothing
        addProvide groupid_2 "libresolv.so.2(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_2 "libresolv.so.2(GLIBC_2.9)(64bit)" Nothing
        addProvide groupid_2 "rtld(GNU_HASH)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "kernel" (Just "kernel < 2.6.32") groupid_2
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "binutils" (Just "binutils < 2.19.51.0.10") groupid_2
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "prelink" (Just "prelink < 0.4.2") groupid_2
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "glibc-profile" (Just "glibc-profile < 2.4") groupid_2
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "nss_db" (Just "nss_db") groupid_2

        -- self-provided
        addReq groupid_2 "config(glibc) = 2.17-78.el7"
        addReq groupid_2 "ld-linux-x86-64.so.2()(64bit)"
        addReq groupid_2 "ld-linux-x86-64.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_2 "ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)"
        addReq groupid_2 "libCNS.so()(64bit)"
        addReq groupid_2 "libGB.so()(64bit)"
        addReq groupid_2 "libISOIR165.so()(64bit)"
        addReq groupid_2 "libJIS.so()(64bit)"
        addReq groupid_2 "libJISX0213.so()(64bit)"
        addReq groupid_2 "libKSC.so()(64bit)"
        addReq groupid_2 "libc.so.6()(64bit)"
        addReq groupid_2 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_2 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_2 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_2 "libc.so.6(GLIBC_2.3.2)(64bit)"
        addReq groupid_2 "libc.so.6(GLIBC_2.3.3)(64bit)"
        addReq groupid_2 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_2 "libdl.so.2()(64bit)"
        addReq groupid_2 "libdl.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_2 "libnsl.so.1()(64bit)"
        addReq groupid_2 "libnsl.so.1(GLIBC_2.2.5)(64bit)"
        addReq groupid_2 "libnss_files.so.2()(64bit)"
        addReq groupid_2 "libpthread.so.0()(64bit)"
        addReq groupid_2 "libpthread.so.0(GLIBC_2.2.5)(64bit)"
        addReq groupid_2 "libresolv.so.2()(64bit)"
        addReq groupid_2 "libresolv.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_2 "libresolv.so.2(GLIBC_2.9)(64bit)"

        -- glibc-common (id 3)
        addReq groupid_2 "glibc-common = 2.17-78.el7"

        -- nss-softokn-freebl (id 11)
        addReq groupid_2 "libfreebl3.so()(64bit)"
        addReq groupid_2 "libfreebl3.so(NSSRAWHASH_3.12.3)(64bit)"

        let groupid_3 = toSqlKey 3
        insertNEVRA groupid_3 "glibc-common" Nothing "2.17" "78.el7" "x86_64"
        addProvide groupid_3 "config(glibc-common)" (Just "= 2.17-78.el7")

        -- self-provided
        addReq groupid_3 "config(glibc-common) = 2.17-78.el7"

        -- provided by bash (id 1)
        addReq groupid_3 "/bin/sh"
        addReq groupid_3 "/usr/bin/bash"

        -- provided by glibc (id 2)
        addReq groupid_3 "glibc = 2.17-78.el7"
        addReq groupid_3 "libc.so.6()(64bit)"
        addReq groupid_3 "libc.so.6(GLIBC_2.10)(64bit)"
        addReq groupid_3 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_3 "libc.so.6(GLIBC_2.15)(64bit)"
        addReq groupid_3 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_3 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_3 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_3 "libdl.so.2()(64bit)"
        addReq groupid_3 "libdl.so.2(GLIBC_2.2.5)(64bit)"

        -- libselinux (id 4)
        addReq groupid_3 "libselinux.so.1()(64bit)"

        -- tzdata (id 10)
        addReq groupid_3 "tzdata >= 2003a"

        let groupid_4 = toSqlKey 4
        insertNEVRA groupid_4 "libselinux" Nothing "2.2.2" "6.el7" "x86_64"
        addProvide groupid_4 "libselinux.so.1()(64bit)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "filesystem" (Just "filesystem < 3") groupid_4

        -- self-provided
        addReq groupid_4 "libselinux.so.1()(64bit)"

        -- glibc (id 2)
        addReq groupid_4 "ld-linux-x86-64.so.2()(64bit)"
        addReq groupid_4 "ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)"
        addReq groupid_4 "libc.so.6()(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.3.2)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.7)(64bit)"
        addReq groupid_4 "libc.so.6(GLIBC_2.8)(64bit)"
        addReq groupid_4 "libdl.so.2()(64bit)"
        addReq groupid_4 "libdl.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_4 "rtld(GNU_HASH)"

        -- xz-libs (id 5)
        addReq groupid_4 "liblzma.so.5()(64bit)"
        addReq groupid_4 "liblzma.so.5(XZ_5.0)(64bit)"

        -- pcre (id 6)
        addReq groupid_4 "libpcre.so.1()(64bit)"
        addReq groupid_4 "pcre"

        -- libsepol (id 9)
        addReq groupid_4 "libsepol >= 2.1.9-1"

        let groupid_5 = toSqlKey 5
        insertNEVRA groupid_5 "xz-libs" Nothing "5.1.2" "9alpha.el7" "x86_64"
        addProvide groupid_5 "liblzma.so.5()(64bit)" Nothing
        addProvide groupid_5 "liblzma.so.5(XZ_5.0)(64bit)" Nothing

        -- provided by glibc (id 2)
        addReq groupid_5 "libc.so.6()(64bit)"
        addReq groupid_5 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_5 "libc.so.6(GLIBC_2.17)(64bit)"
        addReq groupid_5 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_5 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_5 "libpthread.so.0()(64bit)"
        addReq groupid_5 "libpthread.so.0(GLIBC_2.2.5)(64bit)"
        addReq groupid_5 "libpthread.so.0(GLIBC_2.3.2)(64bit)"
        addReq groupid_5 "libpthread.so.0(GLIBC_2.3.3)(64bit)"
        addReq groupid_5 "rtld(GNU_HASH)"

        let groupid_6 = toSqlKey 6
        insertNEVRA groupid_6 "pcre" Nothing "8.32" "14.el7" "x86_64"
        addProvide groupid_6 "libpcre.so.1()(64bit)" Nothing

        -- self-provided
        addReq groupid_6 "libpcre.so.1()(64bit)"

        -- provided by glibc (id 2)
        addReq groupid_6 "libc.so.6()(64bit)"
        addReq groupid_6 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_6 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_6 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_6 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_6 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_6 "libm.so.6()(64bit)"
        addReq groupid_6 "libpthread.so.0()(64bit)"
        addReq groupid_6 "libpthread.so.0(GLIBC_2.2.5)(64bit)"
        addReq groupid_6 "rtld(GNU_HASH)"

        -- libgcc (id 7)
        addReq groupid_6 "libgcc_s.so.1()(64bit)"
        addReq groupid_6 "libgcc_s.so.1(GCC_3.0)(64bit)"

        -- libstdc++ (id 8)
        addReq groupid_6 "libstdc++.so.6()(64bit)"
        addReq groupid_6 "libstdc++.so.6(CXXABI_1.3)(64bit)"
        addReq groupid_6 "libstdc++.so.6(GLIBCXX_3.4)(64bit)"
        addReq groupid_6 "libstdc++.so.6(GLIBCXX_3.4.9)(64bit)"

        let groupid_7 = toSqlKey 7
        insertNEVRA groupid_7 "libgcc" Nothing "4.8.3" "9.el7" "x86_64"
        addProvide groupid_7 "libgcc_s.so.1()(64bit)" Nothing
        addProvide groupid_7 "libgcc_s.so.1(GCC_3.0)(64bit)" Nothing
        addProvide groupid_7 "libgcc_s.so.1(GCC_3.3)(64bit)" Nothing
        addProvide groupid_7 "libgcc_s.so.1(GCC_4.2.0)(64bit)" Nothing
        -- no requirements

        let groupid_8 = toSqlKey 8
        insertNEVRA groupid_8 "libstdc++" Nothing "4.8.3" "9.el7" "x86_64"
        addProvide groupid_8 "libstdc++" (Just "= 4.8.2-16.el7")
        addProvide groupid_8 "libstdc++.so.6()(64bit)" Nothing
        addProvide groupid_8 "libstdc++.so.6(CXXABI_1.3)(64bit)" Nothing
        addProvide groupid_8 "libstdc++.so.6(GLIBCXX_3.4)(64bit)" Nothing
        addProvide groupid_8 "libstdc++.so.6(GLIBCXX_3.4.9)(64bit)" Nothing

        -- glibc (id 2)
        addReq groupid_8 "glibc >= 2.10.90-7"
        addReq groupid_8 "ld-linux-x86-64.so.2()(64bit)"
        addReq groupid_8 "ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)"
        addReq groupid_8 "libc.so.6()(64bit)"
        addReq groupid_8 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_8 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_8 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_8 "libc.so.6(GLIBC_2.3.2)(64bit)"
        addReq groupid_8 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_8 "libm.so.6()(64bit)"
        addReq groupid_8 "libm.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_8 "rtld(GNU_HASH)"

        -- libgcc (id 7)
        addReq groupid_8 "libgcc_s.so.1()(64bit)"
        addReq groupid_8 "libgcc_s.so.1(GCC_3.0)(64bit)"
        addReq groupid_8 "libgcc_s.so.1(GCC_3.3)(64bit)"
        addReq groupid_8 "libgcc_s.so.1(GCC_4.2.0)(64bit)"

        let groupid_9 = toSqlKey 9
        insertNEVRA groupid_9 "libsepol" Nothing "2.1.9" "3.el7" "x86_64"
        addProvide groupid_9 "libsepol.so.1()(64bit)" Nothing

        -- provided by glibc (id 2)
        addReq groupid_9 "libc.so.6()(64bit)"
        addReq groupid_9 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_9 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_9 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_9 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_9 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_9 "rtld(GNU_HASH)"

        let groupid_10 = toSqlKey 10
        insertNEVRA groupid_10 "tzdata" Nothing "2015a" "1.el7" "noarch"
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "glibc-common" (Just "glibc-common <= 2.3.2-63") groupid_10
        -- no requirements

        let groupid_11 = toSqlKey 11
        insertNEVRA groupid_11 "nss-softokn-freebl" Nothing "3.16.2.3" "9.el7" "x86_64"
        addProvide groupid_11 "libfreebl3.so()(64bit)" Nothing
        addProvide groupid_11 "libfreebl3.so(NSSRAWHASH_3.12.3)(64bit)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "nss" (Just "nss < 3.12.2.99.3-5") groupid_11
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "prelink" (Just "prelink < 0.4.3") groupid_11
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "filesystem" (Just "filesystem < 3") groupid_11

        -- bash (id 1)
        addReq groupid_11 "/bin/bash"

        -- glibc (id 2)
        addReq groupid_11 "libc.so.6()(64bit)"
        addReq groupid_11 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_11 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_11 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_11 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_11 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_11 "libdl.so.2()(64bit)"
        addReq groupid_11 "libdl.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_11 "rtld(GNU_HASH)"

        let groupid_12 = toSqlKey 12
        insertNEVRA groupid_12 "ncurses-libs" Nothing "5.9" "13.20130511.el7" "x86_64"
        addProvide groupid_12 "libform.so.5()(64bit)" Nothing
        addProvide groupid_12 "libformw.so.5()(64bit)" Nothing
        addProvide groupid_12 "libmenu.so.5()(64bit)" Nothing
        addProvide groupid_12 "libmenuw.so.5()(64bit)" Nothing
        addProvide groupid_12 "libncurses.so.5()(64bit)" Nothing
        addProvide groupid_12 "libncursesw.so.5()(64bit)" Nothing
        addProvide groupid_12 "libpanel.so.5()(64bit)" Nothing
        addProvide groupid_12 "libpanelw.so.5()(64bit)" Nothing
        addProvide groupid_12 "libtinfo.so.5()(64bit)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "ncurses" (Just "ncurses < 5.6.13") groupid_12
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "ncurses" (Just "ncurses < 5.6.13") groupid_12
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "libtermcap" (Just "libtermcap < 2.0.8-48") groupid_12

        -- self-provided
        addReq groupid_12 "libform.so.5()(64bit)"
        addReq groupid_12 "libformw.so.5()(64bit)"
        addReq groupid_12 "libmenu.so.5()(64bit)"
        addReq groupid_12 "libmenuw.so.5()(64bit)"
        addReq groupid_12 "libncurses.so.5()(64bit)"
        addReq groupid_12 "libncursesw.so.5()(64bit)"
        addReq groupid_12 "libpanel.so.5()(64bit)"
        addReq groupid_12 "libpanelw.so.5()(64bit)"
        addReq groupid_12 "libtinfo.so.5()(64bit)"

        -- glibc (id 2)
        addReq groupid_12 "libc.so.6()(64bit)"
        addReq groupid_12 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_12 "libc.so.6(GLIBC_2.15)(64bit)"
        addReq groupid_12 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_12 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_12 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_12 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_12 "libdl.so.2()(64bit)"
        addReq groupid_12 "libdl.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_12 "libm.so.6()(64bit)"
        addReq groupid_12 "rtld(GNU_HASH)"

        -- libgcc (id 7)
        addReq groupid_12 "libgcc_s.so.1()(64bit)"
        addReq groupid_12 "libgcc_s.so.1(GCC_3.0)(64bit)"

        -- libstdc++ (id 8)
        addReq groupid_12 "libstdc++.so.6()(64bit)"
        addReq groupid_12 "libstdc++.so.6(CXXABI_1.3)(64bit)"
        addReq groupid_12 "libstdc++.so.6(GLIBCXX_3.4)(64bit)"

        -- ncurses-base (id 13)
        addReq groupid_12 "ncurses-base = 5.9-13.20130511.el7"

        let groupid_13 = toSqlKey 13
        insertNEVRA groupid_13 "ncurses-base" Nothing "5.9" "13.20130511.el7" "noarch"
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "ncurses" (Just "ncurses < 5.6-13") groupid_13
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "filesystem" (Just "filesystem < 3") groupid_13
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "termcap" (Just "termcap < 1:5.5-2") groupid_13
        -- no requirements

        let groupid_14 = toSqlKey 14
        insertNEVRA groupid_14 "tar" (Just "2") "1.26" "29.el7" "x86_64"

        -- glibc (id 2)
        addReq groupid_14 "libc.so.6()(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.17)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.6)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.7)(64bit)"
        addReq groupid_14 "libc.so.6(GLIBC_2.8)(64bit)"
        addReq groupid_14 "rtld(GNU_HASH)"

        -- libselinux (id 4)
        addReq groupid_14 "libselinux.so.1()(64bit)"

        -- libacl (id 15)
        addReq groupid_14 "libacl.so.1()(64bit)"
        addReq groupid_14 "libacl.so.1(ACL_1.0)(64bit)"

        let groupid_15 = toSqlKey 15
        insertNEVRA groupid_15 "libacl" Nothing "2.2.51" "12.el7" "x86_64"
        addProvide groupid_15 "libacl.so.1()(64bit)" Nothing
        addProvide groupid_15 "libacl.so.1(ACL_1.0)(64bit)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "filesystem" (Just "filesystem < 3") groupid_15

        -- glibc (id 2)
        addReq groupid_15 "libc.so.6()(64bit)"
        addReq groupid_15 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_15 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_15 "libc.so.6(GLIBC_2.3.4)(64bit)"
        addReq groupid_15 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_15 "rtld(GNU_HASH)"

        -- libattr (id 16)
        addReq groupid_15 "libattr.so.1()(64bit)"
        addReq groupid_15 "libattr.so.1(ATTR_1.0)(64bit)"

        let groupid_16 = toSqlKey 16
        insertNEVRA groupid_16 "libattr" Nothing "2.4.46" "12.el7" "x86_64"
        addProvide groupid_16 "libattr.so.1()(64bit)" Nothing
        addProvide groupid_16 "libattr.so.1(ATTR_1.0)(64bit)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "filesystem" (Just "filesystem < 3") groupid_16

        -- glibc (id 2)
        addReq groupid_16 "libc.so.6()(64bit)"
        addReq groupid_16 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_16 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_16 "rtld(GNU_HASH)"

    addFile :: MonadIO m => Key Groups -> T.Text -> SqlPersistT m ()
    addFile groupid path = do
        fid <- insert $ Files path "root" "root" 0 (Just "checksum") 0 0 Nothing
        void $ insert $ GroupFiles groupid fid

    addReq :: MonadIO m => Key Groups -> T.Text -> SqlPersistT m ()
    addReq groupid expr = do
        reqid <- insertRequirement $ mkRequirement RT.Runtime RT.Must expr
        void $ insertGroupRequirement $ mkGroupRequirement groupid reqid

    addProvide :: MonadIO m => Key Groups -> T.Text -> Maybe T.Text -> SqlPersistT m ()
    addProvide groupid depname depver = let
        depext = depname <> maybe "" (" " `T.append`) depver
     in
        void $ insertGroupKeyValue (TextKey "rpm-provide") depname (Just depext) groupid

    insertNEVRA :: MonadIO m => Key Groups -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> T.Text -> SqlPersistT m ()
    insertNEVRA gid name epoch version release arch = do
        insertKey gid $ Groups name "rpm" Nothing
        void $ insertGroupKeyValue (TextKey "name")    name    Nothing gid
        void $ insertGroupKeyValue (TextKey "version") version Nothing gid
        void $ insertGroupKeyValue (TextKey "release") release Nothing gid
        void $ insertGroupKeyValue (TextKey "arch")    arch    Nothing gid
        maybe (return ())
              (\e -> void $ insertGroupKeyValue (TextKey "epoch") e Nothing gid)
              epoch

        -- Add the self-provide
        let evr = maybe "" (T.append ":") epoch <> version <> "-" <> release
        addProvide gid name (Just ("= " <> evr))
        when (arch /= "noarch") $ do
            let archname = name <> "(" <> arch <> ")"
            addProvide gid archname (Just ("= " <> evr))

    withDeps :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT (NoLoggingT (ResourceT (ExceptT e m))) a -> m (Either e a)
    withDeps action = (runExceptT . withDb) (addDeps >> action)

    withRealDeps :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT (NoLoggingT (ResourceT (ExceptT e m))) a -> m (Either e a)
    withRealDeps action = (runExceptT . withDb) (addRealDeps >> action)

    -- Like withRealDeps, but with an extra glibc
    addGlibcUpgrade :: MonadIO m => SqlPersistT m ()
    addGlibcUpgrade = do
        addRealDeps

        let groupid_1000 = toSqlKey 1000
        insertNEVRA groupid_1000 "glibc-upgrade" Nothing "3.0" "1" "x86_64"
        -- Add all of the same provides and requires as glibc-2.17-78.el7.x86_64
        addProvide groupid_1000 "glibc" (Just "= 2.17-78.el7")
        addProvide groupid_1000 "glibc(x86_64)" (Just "= 2.17-78.el7")
        addProvide groupid_1000 "config(glibc)" (Just "= 2.17-78.el7")
        addProvide groupid_1000 "ld-linux-x86-64.so.2()(64bit)" Nothing
        addProvide groupid_1000 "ld-linux-x86-64.so.2(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)" Nothing
        addProvide groupid_1000 "libCNS.so()(64bit)" Nothing
        addProvide groupid_1000 "libGB.so()(64bit)" Nothing
        addProvide groupid_1000 "libISOIR165.so()(64bit)" Nothing
        addProvide groupid_1000 "libJIS.so()(64bit)" Nothing
        addProvide groupid_1000 "libJISX0213.so()(64bit)" Nothing
        addProvide groupid_1000 "libKSC.so()(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6()(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.10)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.11)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.12)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.13)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.14)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.15)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.17)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.3)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.3.2)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.3.3)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.3.4)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.4)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.7)(64bit)" Nothing
        addProvide groupid_1000 "libc.so.6(GLIBC_2.8)(64bit)" Nothing
        addProvide groupid_1000 "libdl.so.2()(64bit)" Nothing
        addProvide groupid_1000 "libdl.so.2(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "libm.so.6()(64bit)" Nothing
        addProvide groupid_1000 "libm.so.6(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "libnsl.so.1()(64bit)" Nothing
        addProvide groupid_1000 "libnsl.so.1(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "libnss_files.so.2()(64bit)" Nothing
        addProvide groupid_1000 "libpthread.so.0()(64bit)" Nothing
        addProvide groupid_1000 "libpthread.so.0(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "libpthread.so.0(GLIBC_2.3.2)(64bit)" Nothing
        addProvide groupid_1000 "libpthread.so.0(GLIBC_2.3.3)(64bit)" Nothing
        addProvide groupid_1000 "libresolv.so.2()(64bit)" Nothing
        addProvide groupid_1000 "libresolv.so.2(GLIBC_2.2.5)(64bit)" Nothing
        addProvide groupid_1000 "libresolv.so.2(GLIBC_2.9)(64bit)" Nothing
        addProvide groupid_1000 "rtld(GNU_HASH)" Nothing
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "kernel" (Just "kernel < 2.6.32") groupid_1000
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "binutils" (Just "binutils < 2.19.51.0.10") groupid_1000
        void $ insertGroupKeyValue (TextKey "rpm-conflict") "prelink" (Just "prelink < 0.4.2") groupid_1000
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "glibc-profile" (Just "glibc-profile < 2.4") groupid_1000
        void $ insertGroupKeyValue (TextKey "rpm-obsolete") "nss_db" (Just "nss_db") groupid_1000
        addReq groupid_1000 "config(glibc) = 2.17-78.el7"
        addReq groupid_1000 "ld-linux-x86-64.so.2()(64bit)"
        addReq groupid_1000 "ld-linux-x86-64.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_1000 "ld-linux-x86-64.so.2(GLIBC_2.3)(64bit)"
        addReq groupid_1000 "libCNS.so()(64bit)"
        addReq groupid_1000 "libGB.so()(64bit)"
        addReq groupid_1000 "libISOIR165.so()(64bit)"
        addReq groupid_1000 "libJIS.so()(64bit)"
        addReq groupid_1000 "libJISX0213.so()(64bit)"
        addReq groupid_1000 "libKSC.so()(64bit)"
        addReq groupid_1000 "libc.so.6()(64bit)"
        addReq groupid_1000 "libc.so.6(GLIBC_2.14)(64bit)"
        addReq groupid_1000 "libc.so.6(GLIBC_2.2.5)(64bit)"
        addReq groupid_1000 "libc.so.6(GLIBC_2.3)(64bit)"
        addReq groupid_1000 "libc.so.6(GLIBC_2.3.2)(64bit)"
        addReq groupid_1000 "libc.so.6(GLIBC_2.3.3)(64bit)"
        addReq groupid_1000 "libc.so.6(GLIBC_2.4)(64bit)"
        addReq groupid_1000 "libdl.so.2()(64bit)"
        addReq groupid_1000 "libdl.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_1000 "libnsl.so.1()(64bit)"
        addReq groupid_1000 "libnsl.so.1(GLIBC_2.2.5)(64bit)"
        addReq groupid_1000 "libnss_files.so.2()(64bit)"
        addReq groupid_1000 "libpthread.so.0()(64bit)"
        addReq groupid_1000 "libpthread.so.0(GLIBC_2.2.5)(64bit)"
        addReq groupid_1000 "libresolv.so.2()(64bit)"
        addReq groupid_1000 "libresolv.so.2(GLIBC_2.2.5)(64bit)"
        addReq groupid_1000 "libresolv.so.2(GLIBC_2.9)(64bit)"
        addReq groupid_1000 "glibc-common = 2.17-78.el7"
        addReq groupid_1000 "libfreebl3.so()(64bit)"
        addReq groupid_1000 "libfreebl3.so(NSSRAWHASH_3.12.3)(64bit)"

    withGlibcUpgrade :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT (NoLoggingT (ResourceT (ExceptT e m))) a -> m (Either e a)
    withGlibcUpgrade action = (runExceptT . withDb) (addGlibcUpgrade >> action)
