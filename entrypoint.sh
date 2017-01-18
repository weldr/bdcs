#!/bin/bash

sqlite3 /mddb/metadata.db < /root/schema.sql
/usr/local/bin/import /mddb/metadata.db /rpms/*rpm
