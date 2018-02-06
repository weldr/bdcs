#!/bin/bash
# Note: execute from the project root directory

. /usr/share/beakerlib/beakerlib.sh || exit 1

BDCS="./dist/build/bdcs/bdcs"
METADATA_DB="metadata.db"
CS_REPO="/tmp/depsolve.repo"

export PATH="./dist/build/bdcs-import:./dist/build/bdcs-depsolve:$PATH"


rlJournalStart
    rlPhaseStartSetup
        rlRun "sqlite3 $METADATA_DB < ./schema.sql"
        DNF_ROOT=`mktemp -d /tmp/dnf.root.XXXXXX`
        DNF_DOWNLOAD=`mktemp -d /tmp/dnf.download.XXXXXX`
    rlPhaseEnd

    rlPhaseStartTest "When executed without parameters shows usage"
        output=`$BDCS depsolve`
        rlAssertEquals "Usage header is as expected" "$output" "Usage: depsolve metadata.db NEVRA [NEVRA ...]"
    rlPhaseEnd

    rlPhaseStartTest "When executed with non-existing DB returns non-zero"
        $BDCS depsolve /tmp/none.db httpd
        rlAssertNotEquals "Return code must be non-zero" $? 0
    rlPhaseEnd

    rlPhaseStartTest "Depsolve sanity"
        rlLogInfo "Depsolve and download all the RPMs for httpd via DNF"
        rlRun "sudo dnf install -y --nogpgcheck --releasever=26 --downloadonly --downloaddir=$DNF_DOWNLOAD --installroot=$DNF_ROOT httpd"

        rlLogInfo "Import all downloaded RPMs"
        for F in $DNF_DOWNLOAD/*.rpm; do
            rlRun -c -t "$BDCS import $METADATA_DB $CS_REPO file://$F"
        done

        # figure out the exact NEVRA for the httpd package
        HTTPD_NEVRA=`find $DNF_DOWNLOAD -type f -name "httpd-2*.rpm" | cut -f4 -d/ | sed 's/\.rpm//'`

        # when called with correct parameters
        # then output contains input nevra
        # and the return code is 0
        OUTPUT=`$BDCS depsolve $METADATA_DB $HTTPD_NEVRA`
        rlAssert0 "depsolve return code must be zero" $?

        echo "$OUTPUT" | grep $HTTPD_NEVRA
        rlAssert0 "httpd included in depsolved list" $?
    rlPhaseEnd

    rlPhaseStartCleanup
        rm -rf $METADATA_DB $CS_REPO $DNF_ROOT $DNF_DOWNLOAD
    rlPhaseEnd
rlJournalEnd
