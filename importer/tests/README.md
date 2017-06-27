Depclose integration tests!
---------------------------

This test suite parses the recipes in `examples/recipes/` and tries to depclose
the resulting package list using the `dist/build/depclose/depclose` binary!
The tests which are performed are:

1) Can we resolve all the dependencies
2) Are the packages the user wanted still in the resolved list
3) Does the resulting recipe provide the intended functionality

**NOTE:** 3) is still not implemented. The intention is to build a Docker image
using the compose and run basic functional tests against that! For example if
the user wanted to build a recipe with an HTTP server so she can host files then
the functional test would be to start the container and actually verify that
there is something listening on port 80 and that it can serve static files!

Installation
============

    $ mkvirtualenv depclose-tests
    (depclose-tests)$ pip install -r requirements.txt

Execution
=========

    (depclose-tests)$ METADATA_DB=~/path/to/metadata.db test_depclose.py


`dist/build/depclose/depclose` needs a `metadata.db` file built by
`dist/build/import/import`. By default it looks for this file into
the current working directory. You can modify the path using the `METADATA_DB`
environment variable.


Metadata DB notes
=================

For publicly available test execution we use a DB prepared from CentOS 7 content,
which includes the following repositories:

* [CentOS 7](http://mirror.centos.org/centos/7/os/x86_64/)
* [CentOS 7 Extras](http://mirror.centos.org/centos/7/extras/x86_64/)

CentOS metadata.db URL: https://s3.amazonaws.com/atodorov/metadata_centos7.db.gz
