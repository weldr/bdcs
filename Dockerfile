FROM fedora:24
RUN dnf -y install sqlite && dnf clean all

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY import /usr/local/bin
COPY schema.sql /root/schema.sql
