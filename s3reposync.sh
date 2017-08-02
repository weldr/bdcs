#!/bin/bash

if [ -z "$1" ]; then
    echo "USAGE: $0 <ostree.repo>"
    exit 1
fi

REPO="$1"

#### NOTE: we don't use --delete-removed because Makefile
#### does an ostree pull with --depth=0 by default, which syncs
#### only the last commit!!!!

# first sync files which are above $REPO/objects/??/
for f in `find $REPO -maxdepth 2 -type f`; do
    TARGET=`echo $f | sed 's|\./||'`
    s3cmd sync -v --no-guess-mime-type -P $f s3://weldr/$TARGET
done

# next sync files under $REPO/objects/??/
for d in `find $REPO -mindepth 2 -maxdepth 2 -type d -print | sort`; do
    BASENAME=`basename $d`
    TARGET=`echo $d | sed 's|\./||' | sed "s/$BASENAME//"`
    s3cmd sync -v --no-guess-mime-type -P $d s3://weldr/$TARGET
done
