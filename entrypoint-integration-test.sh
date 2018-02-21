#!/bin/bash

set -ex

# first build the application
make hlint tests install

./tests/test_tmpfiles.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

./tests/test_import.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

./tests/test_export.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

./tests/test_depsolve.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

# collect coverage data from unit tests and binaries
mkdir ./dist/hpc/vanilla/tix/bdcs/
mv bdcs.tix ./dist/hpc/vanilla/tix/bdcs/
