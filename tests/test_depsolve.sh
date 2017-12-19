#!/bin/bash
# Note: execute from the project root directory

set -x

BDCS="./dist/build/bdcs/bdcs"

# when executed without parameters shows usage
if [[ `$BDCS depsolve` != "Usage: depsolve metadata.db NEVRA [NEVRA ...]" ]]; then
    exit 1
fi

# when executed with non-existing DB returns non-zero
$BDCS depsolve /tmp/none.db httpd
if [[ $? == 0 ]]; then
    echo "FAIL: Return code is zero"
    exit 1
fi

### Prepare for testing depsolve against recipes

METADATA_DB="metadata.db"
CS_REPO="/tmp/depsolve.repo"
sqlite3 $METADATA_DB < ../schema.sql

DNF_ROOT=`mktemp -d /tmp/dnf.root.XXXXXX`
DNF_DOWNLOAD=`mktemp -d /tmp/dnf.download.XXXXXX`

# first depsolve via dnf and download all the RPMs
sudo dnf install -y --nogpgcheck --releasever=26 --downloadonly --downloaddir=$DNF_DOWNLOAD --installroot=$DNF_ROOT httpd

# then import all RPMs
for F in $DNF_DOWNLOAD/*.rpm; do
    $BDCS import $METADATA_DB $CS_REPO file://$F
done

# figure out the exact NEVRA for the httpd package
HTTPD_NEVRA=`find $DNF_DOWNLOAD -type f -name "httpd-2*.rpm" | cut -f4 -d/ | sed 's/\.rpm//'`


# when called with correct parameters
# then output contains input nevra
# and the return code is 0
OUTPUT=`$BDCS depsolve $METADATA_DB $HTTPD_NEVRA`
if [[ $? != 0 ]]; then
    echo "FAIL: Return code is not zero"
    exit 1
fi

echo "$OUTPUT" | grep $HTTPD_NEVRA
if [ $? != 0 ]; then
    echo "FAIL: httpd not included in depsolved list"
    exit 1
fi
