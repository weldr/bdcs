#!/bin/bash

STORE=$(realpath /mddb/${STORE:-cs.repo})
MDDB="/mddb/${MDDB:-metadata.db}"

if [[ -e "$STORE" && -z "$KEEP_STORE" && "$STORE" =~ ^/mddb/ ]]; then
    rm -rf "$STORE"
fi

if [[ -e "$MDDB" && -z "$KEEP_MDDB" ]]; then
    rm "$MDDB"
fi

if [ ! -f "$MDDB" ]; then
    sqlite3 "$MDDB" < /root/schema.sql
fi

for f in /rpms/*rpm; do
    /usr/local/bin/bdcs import "$MDDB" "$STORE" file://${f}
done

# if URL was passed try to import from there
if [ -n "$IMPORT_URL" ]; then
    /usr/local/bin/bdcs import "$MDDB" "$STORE" $IMPORT_URL
fi
