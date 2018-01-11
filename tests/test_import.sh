#!/bin/bash
# Note: execute from the project root directory

. /usr/share/beakerlib/beakerlib.sh || exit 1

BDCS="./dist/build/bdcs/bdcs"
METADATA_DB="./import_metadata.db"
CENTOS_REPO="centos.repo"
export PATH="./dist/build/bdcs-import:$PATH"

rlJournalStart
    rlPhaseStartSetup
        rlRun "sqlite3 $METADATA_DB < ./schema.sql"
    rlPhaseEnd

    rlPhaseStartTest "When executed without parameters shows usage"
        output=`$BDCS import | head -n 2 | tail -n 1`
        rlAssertEquals "Usage header as expected" "$output" "Usage: import output.db repo thing [thing ...]"
    rlPhaseEnd

    rlPhaseStartTest "When importing from URL which gives a 404 exit code should be non-zero"
        $BDCS import $METADATA_DB $CENTOS_REPO http://mirror.centos.org/centos/7/os/x86_64/Packages/NON-EXISTING-RPM.rpm
        rlAssertNotEquals "On error exit code should not be zero" $? 0
    rlPhaseEnd

    rlPhaseStartTest "Can import a single RPM from local disk"
        rlRun -t -c "wget http://mirror.centos.org/centos/7/os/x86_64/Packages/setup-2.8.71-7.el7.noarch.rpm"
        rlRun -t -c "$BDCS import $METADATA_DB $CENTOS_REPO file://`pwd`/setup-2.8.71-7.el7.noarch.rpm"
    rlPhaseEnd

    rlPhaseStartTest "Can import a single RPM from HTTPS URL"
        rlRun -t -c "$BDCS import $METADATA_DB $CENTOS_REPO https://download-ib01.fedoraproject.org/pub/epel/7/x86_64/Packages/a/abc-1.01-9.hg20160905.el7.x86_64.rpm"
    rlPhaseEnd

    rlPhaseStartTest "Can import entire repository from HTTPS URL"
        rlRun -t -c "$BDCS import $METADATA_DB $CENTOS_REPO https://s3.amazonaws.com/weldr/https-import-repo/"
    rlPhaseEnd

    rlPhaseStartCleanup
        rm -rf $CENTOS_REPO $METADATA_DB *.rpm
    rlPhaseEnd
rlJournalEnd
