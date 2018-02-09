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

The Makefile expects that the RPMs are in $PWD/rpms.

After completion, the mddb and content store will be in a bdcs-mddb-volume
docker volume.

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
