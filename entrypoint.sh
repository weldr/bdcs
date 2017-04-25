#!/bin/bash

MDDB="/mddb/${MDDB:-metadata.db}"

if [[ -e "$MDDB" && -z "$KEEP_MDDB" ]]; then
    rm "$MDDB"
fi

if [ ! -f "$MDDB" ]; then
    sqlite3 "$MDDB" < /root/schema.sql
fi

for f in /rpms/*rpm; do
    /usr/local/bin/import "$MDDB" cs.repo file://${f}
done

# if URL was passed try to import from there
if [ -n "$IMPORT_URL" ]; then
    /usr/local/bin/import "$MDDB" cs.repo $IMPORT_URL
fi
