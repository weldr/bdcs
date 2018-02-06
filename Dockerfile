FROM welder/fedora:latest

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY dist/build/bdcs/bdcs /usr/local/bin
COPY dist/build/bdcs-import/bdcs-import /usr/local/libexec/weldr/
COPY dist/build/bdcs-export/bdcs-export /usr/local/libexec/weldr/
COPY schema.sql /root/schema.sql
