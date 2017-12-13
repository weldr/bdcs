#!/bin/bash
# Note: execute from the project root directory

set -x

BDCS="./dist/build/bdcs/bdcs"

function compare_with_rpm() {
    # Verify that contents of an exported directory have the same files as if
    # RPMs were installed via rpm.
    EXPORT_DIR=$1
    shift

    RPMS=$@
    RPM_CHROOT=`mktemp -d /tmp/rpm-chroot.XXXXXX`

    # install the RPMs with rpm
    sudo rpm -Uhv --root $RPM_CHROOT --nodeps --noscripts $RPMS
    if [[ $? != 0 ]]; then
        echo "ERROR: rpm exit code should be zero"
        exit 1
    fi

    for BASE_FILE in `find $RPM_CHROOT -type f`; do
        ### skip some files because the rpm -Uhv will produce
        ### different content for them and cause the test to fail

        if [[ "$BASE_FILE" == */etc/shadow ]]; then
            echo "INFO: skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */etc/gshadow ]]; then
            echo "INFO: skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */etc/passwd ]]; then
            echo "INFO: skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */etc/group ]]; then
            echo "INFO: skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */var/lib/rpm/* ]]; then
            echo "INFO: skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == *.pyc ]]; then
            echo "INFO: skipping $BASE_FILE ..."
            continue
        fi

        echo "INFO: examining $BASE_FILE ..."
        EXPECTED_FILE=`echo $BASE_FILE | sed "s|$RPM_CHROOT|$EXPORT_DIR|" | tr -s '/'`

        if [[ ! -f "$EXPECTED_FILE" ]]; then
            echo "ERROR: $EXPECTED_FILE doesn't exist"
            exit 1
        fi

        BASE_SHA256=`sha256sum $BASE_FILE | cut -f1 -d' '`
        EXPECTED_SHA256=`sha256sum $EXPECTED_FILE | cut -f1 -d' '`

        if [[ $BASE_SHA256 != $EXPECTED_SHA256 ]]; then
            echo "ERROR: $BASE_SHA256($BASE_FILE) != $EXPECTED_SHA256($EXPECTED_FILE)"
            diff -u $BASE_FILE $EXPECTED_FILE
            exit 1
        fi
    done

    sudo rm -rf $RPM_CHROOT
}


# when executed without parameters shows usage
if [[ `$BDCS export | head -n 2 | tail -n 1` != "Usage: export metadata.db repo dest thing [thing ...]" ]]; then
    exit 1
fi

############################################################
### Prepare for testing export functionality

CS_REPO="./export.repo"
METADATA_DB="./export_metadata.db"
EXPORT_DIR="./exported-content.d/"

sqlite3 $METADATA_DB < ../schema.sql

# filesystem package is required by the exporter
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/filesystem-3.2-21.el7.x86_64.rpm && \
    $BDCS import $METADATA_DB $CS_REPO file://`pwd`/filesystem-3.2-21.el7.x86_64.rpm
# setup package is required since 5834760
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/setup-2.8.71-7.el7.noarch.rpm && \
    $BDCS import $METADATA_DB $CS_REPO file://`pwd`/setup-2.8.71-7.el7.noarch.rpm

############################################################
## When exporting a non-existing package
## Then returns an error

OUTPUT=`sudo $BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 NON-EXISTING`
if [[ $? == 0 ]]; then
    echo "ERROR: On error exit code should not be zero"
    exit 1
fi

if [[ "$OUTPUT" != '"No such group NON-EXISTING"' ]]; then
    echo "ERROR: Error output doesn't match"
    exit 1
fi

sudo rm -rf $EXPORT_DIR

############################################################
## When exporting existing package
## Then exported contents match what's inside the RPM package

wget http://mirror.centos.org/centos/7/os/x86_64/Packages/yum-rhn-plugin-2.0.1-9.el7.noarch.rpm && \
    $BDCS import $METADATA_DB $CS_REPO file://`pwd`/yum-rhn-plugin-2.0.1-9.el7.noarch.rpm

sudo $BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch yum-rhn-plugin-2.0.1-9.el7.noarch
if [[ $? != 0 ]]; then
    echo "ERROR: Exit code should be zero"
    exit 1
fi

compare_with_rpm $EXPORT_DIR filesystem-3.2-21.el7.x86_64.rpm setup-2.8.71-7.el7.noarch.rpm yum-rhn-plugin-2.0.1-9.el7.noarch.rpm
sudo rm -rf $EXPORT_DIR

############################################################
## When exporting existing package into .tar image
## Then untarred contents match the contents of RPM

sudo $BDCS export $METADATA_DB $CS_REPO exported.tar filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch yum-rhn-plugin-2.0.1-9.el7.noarch
if [[ $? != 0 ]]; then
    echo "ERROR: Exit code should be zero"
    exit 1
fi

mkdir tar_contents && pushd tar_contents/ && tar xvf ../exported.tar && popd
compare_with_rpm tar_contents/ filesystem-3.2-21.el7.x86_64.rpm setup-2.8.71-7.el7.noarch.rpm yum-rhn-plugin-2.0.1-9.el7.noarch.rpm
sudo rm -rf tar_contents/ exported.tar


############################################################
## When exporting two conflicting packages
## And they conflict on a symlink
## Then (since 5834760) the first symlink to be exported wins

# in libcmpiCppImpl0:
# libcmpiCppImpl.so and libcmpiCppImpl.so.0 are symlinks to libcmpiCppImpl.so.0.0.0

# in tog-pegasus-libs:
# libcmpiCppImpl.so is a symlink to libcmpiCppImpl.so.1

# the conflicting file is the libcmpiCppImpl.so symlink


# these two packages both provide /usr/lib64/libcmpiCppImpl.so
# normally libcmpiCppImpl0 lives in the @conflicts group
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/tog-pegasus-libs-2.14.1-5.el7.x86_64.rpm && \
    $BDCS import $METADATA_DB $CS_REPO file://`pwd`/tog-pegasus-libs-2.14.1-5.el7.x86_64.rpm
wget http://mirror.centos.org/centos/7/os/x86_64/Packages/libcmpiCppImpl0-2.0.3-5.el7.x86_64.rpm && \
    $BDCS import $METADATA_DB $CS_REPO file://`pwd`/libcmpiCppImpl0-2.0.3-5.el7.x86_64.rpm


# first libcmpiCppImpl0, second tog-pegasus-libs
sudo $BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch libcmpiCppImpl0-2.0.3-5.el7.x86_64 tog-pegasus-libs-2:2.14.1-5.el7.x86_64 2>&1
if [[ $? != 0 ]]; then
    echo "ERROR: Exit code should be zero"
    exit 1
fi

# conflict is in tog-pegasus-libs which is the second package in the list
# make sure libcmpiCppImpl0 wins
compare_with_rpm $EXPORT_DIR filesystem-3.2-21.el7.x86_64.rpm setup-2.8.71-7.el7.noarch.rpm libcmpiCppImpl0-2.0.3-5.el7.x86_64.rpm
sudo rm -rf $EXPORT_DIR


# first tog-pegasus-libs, second libcmpiCppImpl0
sudo $BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch tog-pegasus-libs-2:2.14.1-5.el7.x86_64 libcmpiCppImpl0-2.0.3-5.el7.x86_64
if [[ $? != 0 ]]; then
    echo "ERROR: Exit code should be zero"
    exit 1
fi

# conflict is in libcmpiCppImpl0 which is the second package in the list
# make sure tog-pegasus wins
compare_with_rpm $EXPORT_DIR filesystem-3.2-21.el7.x86_64.rpm setup-2.8.71-7.el7.noarch.rpm tog-pegasus-libs-2.14.1-5.el7.x86_64.rpm
sudo rm -rf $EXPORT_DIR
