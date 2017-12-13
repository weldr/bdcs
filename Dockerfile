FROM welder/fedora:latest

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY bdcs /usr/local/bin
COPY bdcs-import /usr/local/libexec/weldr/
COPY bdcs-export /usr/local/libexec/weldr/
COPY schema.sql /root/schema.sql
