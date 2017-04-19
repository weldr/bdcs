#!/bin/bash

if [ ! -f /mddb/metadata.db ]; then
    sqlite3 /mddb/metadata.db < /root/schema.sql
fi

for f in /rpms/*rpm; do
    /usr/local/bin/import /mddb/metadata.db cs.repo file://${f}
done
