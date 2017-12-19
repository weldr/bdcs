ifdef WORKSPACE
d = ${WORKSPACE}
else
d = ${CURDIR}
endif

ORG_NAME=welder
STORE ?= cs.repo
MDDB ?= metadata.db

weld-f25:
	git clone https://github.com/weldr/welder-deployment
	$(MAKE) -C welder-deployment weld-f25
	-rm -rf ./welder-deployment

build: Dockerfile.build
	if [ -n "$$TRAVIS" ]; then \
	    sudo docker build -t $(ORG_NAME)/bdcs-build-img -f $< --cache-from $(ORG_NAME)/bdcs-build-img:latest .; \
	else \
	    sudo docker build -t $(ORG_NAME)/bdcs-build-img -f $< .; \
	fi;
	sudo docker create --name build-cont $(ORG_NAME)/bdcs-build-img
	sudo docker cp build-cont:/usr/local/bin/bdcs ./bdcs
	sudo docker cp build-cont:/usr/local/libexec/weldr/bdcs-import ./bdcs-import
	sudo docker cp build-cont:/usr/local/libexec/weldr/bdcs-export ./bdcs-export
	sudo docker rm build-cont

importer: build
	sudo docker build -t $(ORG_NAME)/bdcs-import-img .

integration-test: build Dockerfile.integration-test
	sudo docker build -t $(ORG_NAME)/bdcs-integration-test -f Dockerfile.integration-test .
	sudo docker run --name tests $(ORG_NAME)/bdcs-integration-test


# NOTE: The mddb and content store under ./mddb/ will be removed
#       Unless KEEP_STORE=1 and KEEP_MDDB=1 are set.
mddb:
	@if [ ! -e ${d}/mddb ]; then \
	    mkdir ${d}/mddb; \
	fi;
	sudo docker rm -f mddb-container || true
	sudo docker run -v ${d}/mddb/:/mddb/ -v ${d}/rpms:/rpms:ro --security-opt="label=disable" \
	    --name mddb-container         \
	    -e "IMPORT_URL=$(IMPORT_URL)" \
	    -e "KEEP_STORE=$(KEEP_STORE)"   \
	    -e "STORE=$(STORE)"             \
	    -e "KEEP_MDDB=$(KEEP_MDDB)"   \
	    -e "MDDB=$(MDDB)"             \
	    $(ORG_NAME)/bdcs-import-img
	sudo docker rm mddb-container

api-mddb:
	@if [ ! -e ${d}/api-rpms ]; then \
	    mkdir ${d}/api-rpms; \
	fi; \
	if [ ! -e ${d}/api-mddb ]; then \
	    mkdir ${d}/api-mddb; \
	fi; \
	cd ${d}/api-rpms; \
	wget -c http://mirror.centos.org/centos/7/os/x86_64/Packages/basesystem-10.0-7.el7.centos.noarch.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/filesystem-3.2-21.el7.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/httpd-2.4.6-67.el7.centos.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/bash-4.2.46-28.el7.x86_64.rpm \
	    http://mirror.centos.org/centos/7/updates/x86_64/Packages/bash-4.2.46-29.el7_4.x86_64.rpm
	sudo docker volume create -d local --name api-test-mddb-volume
	sudo docker run -v ${d}/api-mddb:/mddb:z -v ${d}/api-rpms:/rpms:z,ro --security-opt="label:disable" $(ORG_NAME)/bdcs-import-img


.PHONY: importer mddb api-mddb ci

import-centos7:
	make weld-f25
	make importer
	mkdir rpms/

	if [ ! -e ${d}/mddb ]; then \
	    mkdir ${d}/mddb;        \
	fi;                         \

	if [ -n "$$EXISTING_MDDB" ]; then                          \
	    wget --progress=dot:giga "$$EXISTING_MDDB";            \
	    gunzip -q `basename "$$EXISTING_MDDB"`;                \
	    UNZIPPED=`basename "$$EXISTING_MDDB" | sed 's/.gz//'`; \
	    mv $$UNZIPPED ${d}/mddb/$(MDDB);                       \
	fi

	if [ -n "$$EXISTING_STORE" ]; then                                          \
	    STORE=`basename "$$EXISTING_STORE"`;                                    \
	    ostree --repo=${d}/mddb/$$STORE init --mode=archive;                    \
	    ostree --repo=${d}/mddb/$$STORE remote add --no-gpg-verify existing "$$EXISTING_STORE"; \
	    # note: pulls with --depth=0, only the last commit                      \
	    ostree --repo=${d}/mddb/$$STORE pull --mirror existing;                 \
	fi                                                                          \

	set -e
	for REPO in http://mirror.centos.org/centos/7/os/x86_64/ \
	            http://mirror.centos.org/centos/7/extras/x86_64/; do \
	    export IMPORT_URL="$$REPO"; \
	    export KEEP_STORE=1; \
	    export STORE="$$STORE"; \
	    export KEEP_MDDB=1; \
	    make mddb; \
	done

ci: integration-test

ci_after_success:
	# copy coverage data & compiled binaries out of the container
	sudo docker cp tests:/bdcs/dist ./bdcs/dist
	sudo docker rm tests

	sudo chown travis:travis -R ./bdcs/dist
	[ -x ~/.cabal/bin/hpc-coveralls ] || cabal update && cabal install hpc-coveralls
	cd bdcs/ && ~/.cabal/bin/hpc-coveralls --display-report test-bdcs bdcs bdcs-import bdcs-inspect inspect-groups inspect-ls inspect-nevras bdcs-export bdcs-tmpfiles bdcs-depsolve && cd ..

sandbox:
	[ -d .cabal-sandbox ] || cabal sandbox init

hlint: sandbox
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	[ -x .cabal-sandbox/bin/hlint ] || cabal install hlint
	cabal exec hlint .

tests: sandbox
	cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test
