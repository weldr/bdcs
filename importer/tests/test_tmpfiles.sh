#!/bin/bash
# Note: execute from the project root directory

set -x

CMD="./dist/build/bdcs-tmpfiles/bdcs-tmpfiles"
CFG="./data/tmpfiles-default.conf"
TMPDIR="./test-tmpfiles-$$"

err_cleanup() {
    rm -rf "$TMPDIR"
    exit 1
}

$CMD $CFG $TMPDIR || err_cleanup

[ -d "$TMPDIR" ] || err_cleanup
[ -e "$TMPDIR/var/run" ] || err_cleanup
[ -d "$TMPDIR/usr/lib/debug/usr/lib64" ] || err_cleanup

rm -rf "$TMPDIR"
