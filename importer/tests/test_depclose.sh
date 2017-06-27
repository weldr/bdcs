#!/bin/bash
# Note: execute from the project root directory

set -x

DEPCLOSE="./dist/build/depclose/depclose"

# when executed without parameters shows usage
if [[ `$DEPCLOSE | tail -n 1` != "Usage: depclose metadata.db RPM [RPM ...]" ]]; then
    exit 1
fi
