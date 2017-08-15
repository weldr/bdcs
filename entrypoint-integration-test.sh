#!/bin/bash

set -ex

cd /importer/

./tests/test_tmpfiles.sh
./tests/test_import.sh
./tests/test_export.sh

# submit to coveralls
if [ -n "$TRAVIS_JOB_ID" ]; then
    # collect coverage data from unit tests and binaries
    mkdir ./dist/hpc/vanilla/tix/bdcs-tmpfiles/ ./dist/hpc/vanilla/tix/import/ ./dist/hpc/vanilla/tix/export/
    mv bdcs-tmpfiles.tix ./dist/hpc/vanilla/tix/bdcs-tmpfiles/
    mv import.tix ./dist/hpc/vanilla/tix/import/
    mv export.tix ./dist/hpc/vanilla/tix/export/

    hpc-coveralls --display-report test-bdcs import export bdcs-tmpfiles
fi
