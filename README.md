This code requires haskell-rpm which is a separate git repo, but is also
a subtree in this git repo.  So you don't really need to go get it separately.

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
$ cd importer
$ cabal sandbox init
$ cabal sandbox add-source ../haskell-rpm
$ cabal install --dependencies-only --enable-tests
$ cabal build
$ sqlite3 metadata.db < ../schema.sql
$ for f in ${PWD}/Packages/*rpm; do dist/build/import/import metadata.db cs.repo file://${f}; done
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

After completion, the mddb will be in $PWD/mddb/metadata.db.
