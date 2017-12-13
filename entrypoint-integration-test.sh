#!/bin/bash

set -ex

cd /bdcs/

./tests/test_tmpfiles.sh
./tests/test_import.sh
./tests/test_export.sh
./tests/test_depsolve.sh

# collect coverage data from unit tests and binaries
mkdir ./dist/hpc/vanilla/tix/bdcs-tmpfiles/ ./dist/hpc/vanilla/tix/import/ ./dist/hpc/vanilla/tix/export/
mkdir ./dist/hpc/vanilla/tix/bdcs-depsolve/
mv bdcs-tmpfiles.tix ./dist/hpc/vanilla/tix/bdcs-tmpfiles/
mv bdcs-import.tix ./dist/hpc/vanilla/tix/bdcs-import/
mv bdcs-export.tix ./dist/hpc/vanilla/tix/bdcs-export/
mv bdcs-depsolve.tix ./dist/hpc/vanilla/tix/bdcs-depsolve/
