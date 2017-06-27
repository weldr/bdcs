#!/bin/bash
# Note: execute from the project root directory

set -x

IMPORT="./dist/build/import/import"

# when executed without parameters shows usage
if [[ `$IMPORT | head -n 2 | tail -n 1` != "Usage: import output.db repo thing [thing ...]" ]]; then
    exit 1
fi
