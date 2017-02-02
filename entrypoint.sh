#!/bin/bash

if [ -e /mddb/metadata.db ]; then
    rm /mddb/metadata.db
fi

sqlite3 /mddb/metadata.db < /root/schema.sql
/usr/local/bin/import /mddb/metadata.db /rpms/*rpm
