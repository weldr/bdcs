#!/bin/bash
# Note: execute from the project root directory

set -x

# first execute depclose tests
./tests/test_depclose.py
if [[ "$?" != 0 ]]; then
    exit 1
fi

DEPCLOSE="./dist/build/depclose/depclose"

# when executed without parameters shows usage
if [[ `$DEPCLOSE | tail -n 1` != "Usage: depclose metadata.db RPM [RPM ...]" ]]; then
    exit 1
fi
