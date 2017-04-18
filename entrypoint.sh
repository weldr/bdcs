#!/bin/bash

sqlite3 /mddb/metadata.db < /root/schema.sql
for f in /rpms/*rpm; do
    /usr/local/bin/import /mddb/metadata.db cs.repo file://${f}
done
