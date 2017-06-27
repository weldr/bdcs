#!/bin/bash
# Note: execute from the project root directory

set -x

EXPORT="./dist/build/export/export"

# when executed without parameters shows usage
if [[ `$EXPORT | head -n 2 | tail -n 1` != "Usage: export metadata.db repo dest thing [thing ...]" ]]; then
    exit 1
fi
