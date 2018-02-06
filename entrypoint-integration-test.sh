#!/bin/bash

set -ex

cd /bdcs/

./tests/test_tmpfiles.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

./tests/test_import.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

./tests/test_export.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

./tests/test_depsolve.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

# collect coverage data from unit tests and binaries
mkdir ./dist/hpc/vanilla/tix/bdcs-tmpfiles/
mkdir ./dist/hpc/vanilla/tix/bdcs-import/
mkdir ./dist/hpc/vanilla/tix/bdcs-export/
mkdir ./dist/hpc/vanilla/tix/bdcs-depsolve/
mv bdcs-tmpfiles.tix ./dist/hpc/vanilla/tix/bdcs-tmpfiles/
mv bdcs-import.tix ./dist/hpc/vanilla/tix/bdcs-import/
mv bdcs-export.tix ./dist/hpc/vanilla/tix/bdcs-export/
mv bdcs-depsolve.tix ./dist/hpc/vanilla/tix/bdcs-depsolve/
