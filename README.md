[![Build Status](https://travis-ci.org/weldr/bdcs.svg?branch=master)](https://travis-ci.org/weldr/bdcs)
[![Coverage Status](https://coveralls.io/repos/github/weldr/bdcs/badge.svg?branch=master)](https://coveralls.io/github/weldr/bdcs?branch=master)

This code generates a metadata database (mddb) given an input directory of
RPMs.  You can generate this either by running locally or running under docker.
It's really best if you have the RPMs stored locally, too, not under some NFS
mount or other network storage.  That can slow things down quite a bit.

Importing the same set of RPMs into the same database twice should result in no
changes.  Importing additional RPMs into the same database should result in those
RPMs being added to the existing database.  There is currently no provision for
removing an imported RPM.  In this way, you could import a very large set of
packages piecemeal if needed.


Running locally
===============

You will first need a directory full of RPMs somewhere.  Here, I assume that
is the $PWD/Packages directory.  Then run:

```
$ cabal sandbox init
$ cabal install --dependencies-only --enable-tests
$ cabal build
$ sqlite3 metadata.db < schema.sql
$ for f in ${PWD}/Packages/*rpm; do dist/build/bdcs-import/bdcs-import metadata.db cs.repo file://${f}; done
```

Running with docker
===================

Running with docker is a two step process, as indicated by Dockerfile.build
and Dockerfile.  Dockerfile.build is used to compile the program needed to
build an mddb and produces an image with that program.  Dockerfile then runs
that image and produces the mddb.

The Dockerfile depends on a base image, named weld/fedora:25, which needs have
been previously built. If it is not available it can be built from the
welder-deployment repository by running `make weld-f25`.

The Makefile lays out the exact steps and can be used to simplify all this -
just run `make importer mddb`.  If make is unavailable, just copy the steps
out of there and run them manually.

You will first need a volume to store the mddb in and a volume containing all
the RPMs.  The Makefile expects that the mddb will be put into $PWD/mddb, and
that the RPMs are in $PWD/rpms.

After completion, the mddb will be in $PWD/mddb/metadata.db and the ostree
based content store will be in $PWD/mddb/cs.repo/

Preparing local development environment for Haskell
===================================================

For development we use the latest upstream versions:

1) Remove the standard `haskell-platform` and `ghc-*` RPMs if you have them installed
2) Download version **8.0.2** of the generic Haskell Platform distribution from
   https://www.haskell.org/platform/linux.html#linux-generic
3)
```
$ tar -xzvf haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz
$ sudo ./install-haskell-platform.sh
```
4) Add `/usr/local/bin` to your PATH if not already there!
5) Install build dependencies:
```
# dnf -y install xz-devel zlib-devel glib2-devel gobject-introspection-devel ostree-devel
```

**NOTE:** On RHEL 7 `ostree-devel` is part of the Atomic Host product!


Building the project locally
============================

`cabal` is used to install and manage Haskell dependencies from upstream.

```
$ cd src/ && cabal sandbox init && cabal install
```

Executing unit tests
====================

    $ cabal sandbox init
    $ cabal install --dependencies-only --enable-tests
    $ cabal test
    Running 1 test suites...
    Test suite tests: RUNNING...
    Test suite tests: PASS
    Test suite logged to: dist/test/db-0.1.0.0-test-db.log
    1 of 1 test suites (1 of 1 test cases) passed.

Produce code coverage report
============================

    $ cabal sandbox init
    $ cabal install --enable-tests --enable-coverage
    $ cabal test
    $ firefox ./dist/hpc/vanilla/tix/*/hpc_index.html

OSTree cheat sheet
==================

After importing RPMs (via the `import` executable) the results are a
`metadata.db` SQL database and a `cs.repo` directory containing an
OSTree repository. Here are a few quick examples how to work with `ostree`:


To see what references aka "branches" are in the repo:

    ostree --repo=cs.repo/ refs
    master


To see the commit log (use `less` because ostree doesn't paginate like git):

    ostree --repo=cs.repo/ log master
    commit 816b63ab93a7a866d598c552f212c3a407648096521fde74cc9aa317730da8b1
    Date:  2017-07-03 16:32:39 +0000
    
        Import of zsh-html into the repo
    
    commit f3d66bf3b40e269eac7ed62b6d6f7afdeb5059b17e1b8737aec8b52a03ea82c6
    Date:  2017-07-03 16:32:38 +0000
    
        Import of zsh into the repo
    
    commit e370924c48e5b759c05d61b1c4960d96366a91e46eb7449a47c5064986029e50
    Date:  2017-07-03 16:32:35 +0000
    
        Import of yum-rhn-plugin into the repo

To find out the latest commit at `master`:

    ostree --repo=cs.repo/ rev-parse master
    816b63ab93a7a866d598c552f212c3a407648096521fde74cc9aa317730da8b1

To see what files are inside a particular commit:

    ostree --repo=cs.repo/ ls -RC 816b63ab93a7a866d598c552f212c3a407648096521fde74cc9aa317730da8b1
    d00755 0 0      0 30bdab0c9a4156b26a923831f0e1c6ba6141c0a8e35c61f30e8febffe7f78de0 446a0ef11b7cc167f3b603e585c7eeeeb675faa412d5ec73f62988eb0b6c5488 /
    d00755 0 0      0 eeae4a3c7450b786b3d9a6958ef2195c99c1867c2040d9153395e13f38b1ca7b 446a0ef11b7cc167f3b603e585c7eeeeb675faa412d5ec73f62988eb0b6c5488 /usr
    d00755 0 0      0 0e1e668a21df0ef296e0a7287509b9bac8ae2a03bf6849954b3d0ef905b0cd9a 446a0ef11b7cc167f3b603e585c7eeeeb675faa412d5ec73f62988eb0b6c5488 /usr/share
    d00755 0 0      0 d5c72afb0dc8197bba6249a8cdfcf155eed0e1d8bf3d3bd02a4e53f3d8c42b37 446a0ef11b7cc167f3b603e585c7eeeeb675faa412d5ec73f62988eb0b6c5488 /usr/share/doc
    d00755 0 0      0 58efea079328274a2845195529729b190b31b06cc16b5f31c2dc1e2f88825563 446a0ef11b7cc167f3b603e585c7eeeeb675faa412d5ec73f62988eb0b6c5488 /usr/share/doc/zsh-html-5.0.2
    -00644 0 0   1724 6c691ad9ce0f053823e3fe34795d911170bac0f78f9231389c2e9e2e1271f73a /usr/share/doc/zsh-html-5.0.2/Aliasing.html
    -00644 0 0   1892 835ffd08dba662f47c7eb3164ae76aa5903363c63d3d606268c0ba928b725e6e /usr/share/doc/zsh-html-5.0.2/Alternate-Forms-For-Complex-Commands.html
    -00644 0 0   1830 59184f18d807a6b9fc963b709ef82dba4d00f536388935aa79b74160fe90cbfe /usr/share/doc/zsh-html-5.0.2/Alternative-Completion.html
    -00644 0 0   1734 b1bd5813c780a0862843fd3f468021c9358f5852c8a2029cb6857fb64dd2aed1 /usr/share/doc/zsh-html-5.0.2/Arguments.html
    -00644 0 0  17635 89309b924ef3874172c73bab3c45eab56da0b847d6328dc387e0e1c5b3dfa633 /usr/share/doc/zsh-html-5.0.2/Arithmetic-Evaluation.html

**NOTE:** `-R` means recursive, `-C` displays file checksums

To check out aka export a commit into a filesystem tree:

    sudo ostree --repo=cs.repo/ checkout 816b63ab93a7a866d598c552f212c3a407648096521fde74cc9aa317730da8b1 some.dir/
