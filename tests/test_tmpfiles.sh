#!/bin/bash
# Note: execute from the project root directory

. /usr/share/beakerlib/beakerlib.sh || exit 1

CMD="./dist/build/bdcs-tmpfiles/bdcs-tmpfiles"
CFG="./data/tmpfiles-default.conf"
TMPDIR="./test-tmpfiles-$$"

rlJournalStart
    rlPhaseStartTest
        rlRun -t -c "$CMD $CFG $TMPDIR"

        rlAssertExists "$TMPDIR"
        rlAssertExists "$TMPDIR/var/run"
        rlAssertExists "$TMPDIR/usr/lib/debug/usr/lib64"
    rlPhaseEnd

    rlPhaseStartCleanup
        rm -rf $TMPDIR
    rlPhaseEnd
rlJournalEnd
