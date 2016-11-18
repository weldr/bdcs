FROM fedora:24
RUN dnf -y install @c-development gmp-devel ncurses-compat-libs tar xz-devel zlib-devel
RUN cd /tmp && curl -sSL https://downloads.haskell.org/~platform/7.10.3/haskell-platform-7.10.3-unknown-posix-x86_64.tar.gz | tar -xz && ./install-haskell-platform.sh

RUN cabal update

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

COPY haskell-rpm /root/bdcs/haskell-rpm/
COPY db-test /root/bdcs/db-test/

RUN cd /root/bdcs/haskell-rpm && cabal install
RUN cd /root/bdcs/db-test && cabal install
