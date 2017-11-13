#!/bin/bash
# Note: execute from the project root directory

set -x

DEPSOLVE="./dist/build/depsolve/depsolve"

# when executed without parameters shows usage
if [[ `$DEPSOLVE` != "Usage: depsolve metadata.db NEVRA [NEVRA ...]" ]]; then
    exit 1
fi

# when executed with non-existing DB returns non-zero
$DEPSOLVE /tmp/none.db httpd
if [[ $? == 0 ]]; then
    echo "FAIL: Return code is zero"
    exit 1
fi

### Prepare for testing depsolve against recipes

METADATA_DB="metadata.db"
[ -f "$METADATA_DB" ] || curl https://s3.amazonaws.com/weldr/metadata.db > "$METADATA_DB"

# when called with correct parameters
# then output contains input nevra
# and the return code is 0
OUTPUT=`$DEPSOLVE $METADATA_DB httpd-2.4.6-67.el7.centos.x86_64`
if [[ $? != 0 ]]; then
    echo "FAIL: Return code is not zero"
    exit 1
fi

echo "$OUTPUT" | grep httpd-2.4.6-67.el7.centos.x86_64
if [ $? != 0 ]; then
    echo "FAIL: httpd not included in depsolved list"
    exit 1
fi
