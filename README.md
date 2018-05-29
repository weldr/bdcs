The bdcs project is the main backend component of welder.  It is responsible for
managing two major sets of data:  A metadata database (the MDDB) and the content
store.  Collectively, these two are referred to as the BDCS.  This project provides
a set of tools that can be used to generate, add to, and inspect the BDCS.  It
also provides a tool for extracting things from the BDCS, though this is fairly
low level and it's much better to use higher level tools unless you know exactly
what you are doing.

For now, what you store in the BDCS is largely limited to RPMs.  There is some
initial support for storing NPMs and other kinds of packages, but this has not
gotten enough testing to be truly supported.  For the rest of this README, it is
assumed you are only interested in storing RPMs and that they reside in $PWD/rpms.

Your RPMs should be stored locally if at all possible, not under some NFS mount
or other network storage.  Doing so can slow things down quite a bit, though if
your network is fast enough it should be okay.  You can import everything all at
once, or piecemeal.  Attempting to store the same RPM a second time will result
in no changes to the BDCS.  There is currently no way to remove something once
it has been imported.

The top-level tool is simply named `bdcs`.  This command has several subcommands.
See the help output for a current listing.

# Installation & Importing

Before you can do anything with the BDCS, you need to have the bdcs project
built and installed.  You then need to import a bunch of RPMs into the BDCS so
you can do something with it.  There's three basic ways to do this:

## Running locally

The easiest way to run bdcs is if it already exists in your distribution as a
package.  For Fedora 28 and later, just install the ghc-bdcs RPM, which will drag
in the rest of the dependencies.  Other distributions may also include bdcs.

Then simply run:

```
$ sqlite3 metadata.db < schema.sql
$ for f in $PWD/rpms/*.rpm; do bdcs import metadata.db cs.repo file://$f; done
```

## Running from source

If your distribution doesn't have a package, or you want to try out the absolute
latest code, you can always run it from source.  This is only slightly more
involved:

```
$ cabal sandbox init
$ cabal install --dependencies-only
$ cabal install
$ sqlite3 metadata.db < schema.sql
$ for f in ${PWD}/rpms/*rpm; do .cabal-sandbox/bin/bdcs import metadata.db cs.repo file://${f}; done
```

## Running from docker

If for some reason you don't want to install ghc-bdcs and all the Haskell runtime
stuff locally, it's also possible to use docker.  First create the following short
`Dockerfile`:

```
FROM registry.fedoraproject.org/fedora-minimal
RUN microdnf --enablerepo=updates-testing install ghc-bdcs sqlite && microdnf clean all
COPY do-import.sh /usr/local/bin/do-import.sh
```

And create the following `do-import.sh` script in the same directory:

```
#!/bin/bash

if [ ! -e /results/metadata.db ]; then
    sqlite3 /results/metadata.db < /usr/share/bdcs-*/schema.sql
fi

for f in /rpms/*.rpm; do
    bdcs import /results/metadata.db /results/cs.repo file://$f
done
```

Build the image and import your RPMs:

```
$ sudo docker build -t bdcs-import .
$ sudo docker run --entrypoint /usr/local/bin/do-import.sh \
                  -v results:/results \
                  -v $PWD/rpms:/rpms \
                  --security-opt label=disable \
                  --rm -it bdcs-import
```

The resulting mddb (named metadata.db) and content store (named cs.repo) are in the results
docker volume.  In order to use the bdcs subcommands, you should start up the same bdcs-import
container, passing `--entrypoint /bin/bash` to give you a shell, like so:

```
$ sudo docker run --entrypoint /bin/bash \
                  -v results:/results \
                  -v $PWD/rpms:/rpms \
                  --security-opt label=disable \
                  --rm -it bdcs-import
```

# Inspecting

The `inspect` subcommand is used to peek inside the BDCS to see the details of what is stored.
This is a complex command that in turn has its own subcommands.  See the help output or the
`README.inspect.md` document for more details.

# Exporting

The `export` subcommand is used to extract objects from the BDCS and generate a variety of
artifacts like tarballs and qcow images.  This is a low level command that exports only what
you ask for.  No dependency checking is performed, so the resulting artifact may not be
useable.  It is suggested you only use this command if you absolutely know what you are
doing.  Otherwise, use `bdcs-cli`.  See the help output for information on how to use it.
