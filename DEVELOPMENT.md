[![Build Status](https://travis-ci.org/weldr/bdcs.svg?branch=master)](https://travis-ci.org/weldr/bdcs)
[![Coverage Status](https://coveralls.io/repos/github/weldr/bdcs/badge.svg?branch=master)](https://coveralls.io/github/weldr/bdcs?branch=master)


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
