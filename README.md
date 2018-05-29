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
