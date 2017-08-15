#!/bin/bash

set -ex

cd /importer/

./tests/test_tmpfiles.sh
./tests/test_import.sh
./tests/test_export.sh

# collect coverage data from unit tests and binaries
mkdir ./dist/hpc/vanilla/tix/bdcs-tmpfiles/ ./dist/hpc/vanilla/tix/import/ ./dist/hpc/vanilla/tix/export/
mv bdcs-tmpfiles.tix ./dist/hpc/vanilla/tix/bdcs-tmpfiles/
mv import.tix ./dist/hpc/vanilla/tix/import/
mv export.tix ./dist/hpc/vanilla/tix/export/
