#!/bin/bash
# Note: execute from the project root directory

. /usr/share/beakerlib/beakerlib.sh || exit 1

BDCS="./dist/build/bdcs/bdcs"
CS_REPO="./export.repo"
METADATA_DB="./export_metadata.db"
EXPORT_DIR="./exported-content.d/"

export PATH="./dist/build/bdcs-import:./dist/build/bdcs-export:$PATH"


function compare_with_rpm() {
    # Verify that contents of an exported directory have the same files as if
    # RPMs were installed via rpm.
    EXPORT_DIR=$1
    shift

    RPMS=$@
    RPM_CHROOT=`mktemp -d /tmp/rpm-chroot.XXXXXX`

    # install the RPMs with rpm
    rlRun "sudo rpm -Uhv --root $RPM_CHROOT --nodeps --noscripts $RPMS"

    for BASE_FILE in `find $RPM_CHROOT -type f`; do
        ### skip some files because the rpm -Uhv will produce
        ### different content for them and cause the test to fail

        if [[ "$BASE_FILE" == */etc/shadow ]]; then
            rlLogInfo "skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */etc/gshadow ]]; then
            rlLogInfo "skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */etc/passwd ]]; then
            rlLogInfo "skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */etc/group ]]; then
            rlLogInfo "skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == */var/lib/rpm/* ]]; then
            rlLogInfo "skipping $BASE_FILE ..."
            continue
        fi

        if [[ "$BASE_FILE" == *.pyc ]]; then
            rlLogInfo "skipping $BASE_FILE ..."
            continue
        fi

        rlLogInfo "examining $BASE_FILE ..."

        EXPECTED_FILE=`echo $BASE_FILE | sed "s|$RPM_CHROOT|$EXPORT_DIR|" | tr -s '/'`
        rlAssertExists "$EXPECTED_FILE"

        BASE_SHA256=`sha256sum $BASE_FILE | cut -f1 -d' '`
        EXPECTED_SHA256=`sha256sum $EXPECTED_FILE | cut -f1 -d' '`

        if [[ $BASE_SHA256 != $EXPECTED_SHA256 ]]; then
            rlLogError "$BASE_SHA256($BASE_FILE) != $EXPECTED_SHA256($EXPECTED_FILE)"
            rlRun "diff -u $BASE_FILE $EXPECTED_FILE"
            rlFail
        fi
    done

    sudo rm -rf $RPM_CHROOT
}

rlJournalStart
    rlPhaseStartSetup
        rlRun "sqlite3 $METADATA_DB < ./schema.sql"
        # filesystem package is required by the exporter
        rlRun -t -c "wget http://mirror.centos.org/centos/7/os/x86_64/Packages/filesystem-3.2-21.el7.x86_64.rpm"
        rlRun "$BDCS import $METADATA_DB $CS_REPO file://`pwd`/filesystem-3.2-21.el7.x86_64.rpm"

        # setup package is required since 5834760
        rlRun -t -c "wget http://mirror.centos.org/centos/7/os/x86_64/Packages/setup-2.8.71-7.el7.noarch.rpm"
        rlRun "$BDCS import $METADATA_DB $CS_REPO file://`pwd`/setup-2.8.71-7.el7.noarch.rpm"

        rlRun -t -c "wget http://mirror.centos.org/centos/7/os/x86_64/Packages/yum-rhn-plugin-2.0.1-9.el7.noarch.rpm"
        rlRun "$BDCS import $METADATA_DB $CS_REPO file://`pwd`/yum-rhn-plugin-2.0.1-9.el7.noarch.rpm"

        # these two packages both provide /usr/lib64/libcmpiCppImpl.so
        # normally libcmpiCppImpl0 lives in the @conflicts group
        rlRun -t -c "wget http://mirror.centos.org/centos/7/os/x86_64/Packages/tog-pegasus-libs-2.14.1-5.el7.x86_64.rpm"
        rlRun "$BDCS import $METADATA_DB $CS_REPO file://`pwd`/tog-pegasus-libs-2.14.1-5.el7.x86_64.rpm"
        rlRun -t -c "wget http://mirror.centos.org/centos/7/os/x86_64/Packages/libcmpiCppImpl0-2.0.3-5.el7.x86_64.rpm"
        rlRun "$BDCS import $METADATA_DB $CS_REPO file://`pwd`/libcmpiCppImpl0-2.0.3-5.el7.x86_64.rpm"
    rlPhaseEnd

    rlPhaseStartTest "when executed without parameters shows usage"
        OUTPUT=`$BDCS export | head -n 2 | tail -n 1`
        rlAssertEquals "Usage header as expected" "$OUTPUT" "Usage: export metadata.db repo dest thing [thing ...]"
    rlPhaseEnd

    rlPhaseStartTest "When exporting a non-existing package returns an error"
        OUTPUT=`$BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 NON-EXISTING`
        rlAssertNotEquals "On error exit code should not be zero" $? 0
        rlAssertEquals "On error output is as expected" "$OUTPUT" '"No such group NON-EXISTING"'

        sudo rm -rf $EXPORT_DIR
    rlPhaseEnd


    rlPhaseStartTest "When exporting existing package exported contents match what's inside the RPM"
        rlRun "$BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch yum-rhn-plugin-2.0.1-9.el7.noarch"
        compare_with_rpm $EXPORT_DIR filesystem-3.2-21.el7.x86_64.rpm setup-2.8.71-7.el7.noarch.rpm yum-rhn-plugin-2.0.1-9.el7.noarch.rpm
        sudo rm -rf $EXPORT_DIR
    rlPhaseEnd

    rlPhaseStartTest "When exporting existing package into .tar image untarred contents match the contents of RPM"
        rlRun "$BDCS export $METADATA_DB $CS_REPO exported.tar filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch yum-rhn-plugin-2.0.1-9.el7.noarch"

        mkdir tar_contents && pushd tar_contents/ && tar xvf ../exported.tar && popd
        compare_with_rpm tar_contents/ filesystem-3.2-21.el7.x86_64.rpm setup-2.8.71-7.el7.noarch.rpm yum-rhn-plugin-2.0.1-9.el7.noarch.rpm
        sudo rm -rf tar_contents/ exported.tar
    rlPhaseEnd


    rlPhaseStartTest "When exporting two conflicting packages bdcs reports error"
        # in libcmpiCppImpl0:
        # libcmpiCppImpl.so and libcmpiCppImpl.so.0 are symlinks to libcmpiCppImpl.so.0.0.0

        # in tog-pegasus-libs:
        # libcmpiCppImpl.so is a symlink to libcmpiCppImpl.so.1
        OUTPUT=`$BDCS export $METADATA_DB $CS_REPO $EXPORT_DIR filesystem-3.2-21.el7.x86_64 setup-2.8.71-7.el7.noarch libcmpiCppImpl0-2.0.3-5.el7.x86_64 tog-pegasus-libs-2:2.14.1-5.el7.x86_64`
        rlAssertNotEquals "On error exit code should not be zero" $? 0
        rlAssertEquals "On error output is as expected" "$OUTPUT" '"Unable to resolve path /usr/lib64/libcmpiCppImpl.so, non-directory object exists at /usr/lib64/libcmpiCppImpl.so.0.0.0"'

        sudo rm -rf $EXPORT_DIR
    rlPhaseEnd

    rlPhaseStartCleanup
        rm -rf $CS_REPO $METADATA_DB *.rpm $EXPORT_DIR
    rlPhaseEnd
rlJournalEnd
