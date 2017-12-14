#!/bin/bash
# Note: execute from the project root directory

set -x

BDCS="./dist/build/bdcs/bdcs"

# when executed without parameters shows usage
if [[ `$BDCS import | head -n 2 | tail -n 1` != "Usage: import output.db repo thing [thing ...]" ]]; then
    exit 1
fi

METADATA_DB="./import_metadata.db"
sqlite3 $METADATA_DB < ../schema.sql
CENTOS_REPO="centos.repo"

### test that we can import a single RPM from local disk
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/setup-2.8.71-7.el7.noarch.rpm && \
    $BDCS import $METADATA_DB $CENTOS_REPO file://`pwd`/setup-2.8.71-7.el7.noarch.rpm
if [ $? -ne 0 ]; then
    echo "ERROR: importing from local disk failed"
    exit 1
fi

### test that we can import a single RPM from HTTPS URL
$BDCS import $METADATA_DB $CENTOS_REPO https://download-ib01.fedoraproject.org/pub/epel/7/x86_64/a/abc-1.01-9.hg20160905.el7.x86_64.rpm
if [ $? -ne 0 ]; then
    echo "ERROR: importing from HTTPS URL failed"
    exit 1
fi

### test that we can import entire repository from HTTPS URL
$BDCS import $METADATA_DB $CENTOS_REPO https://s3.amazonaws.com/weldr/https-import-repo/
if [ $? -ne 0 ]; then
    echo "ERROR: importing from HTTPS REPO failed"
    exit 1
fi
