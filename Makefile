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
	docker build -t $(ORG_NAME)/bdcs-build-img -f $< --cache-from $(ORG_NAME)/bdcs-build-img:latest .
	docker create --name build-cont $(ORG_NAME)/bdcs-build-img
	docker cp build-cont:/root/.cabal/bin/import ./import
	docker cp build-cont:/root/.cabal/bin/export ./export
	docker rm build-cont

importer: build
	docker build -t $(ORG_NAME)/bdcs-import-img .

integration-test: build Dockerfile.integration-test
	docker build -t $(ORG_NAME)/bdcs-integration-test -f Dockerfile.integration-test .
	docker run --name tests $(ORG_NAME)/bdcs-integration-test


# NOTE: The mddb and content store under ./mddb/ will be removed
#       Unless KEEP_STORE=1 and KEEP_MDDB=1 are set.
mddb:
	@if [ ! -e ${d}/mddb ]; then \
	    mkdir ${d}/mddb; \
	fi;
	docker rm -f mddb-container || true
	docker run -v ${d}/mddb/:/mddb/ -v ${d}/rpms:/rpms:ro --security-opt="label=disable" \
	    --name mddb-container         \
	    -e "IMPORT_URL=$(IMPORT_URL)" \
	    -e "KEEP_STORE=$(KEEP_STORE)"   \
	    -e "STORE=$(STORE)"             \
	    -e "KEEP_MDDB=$(KEEP_MDDB)"   \
	    -e "MDDB=$(MDDB)"             \
	    $(ORG_NAME)/bdcs-import-img
	docker rm mddb-container

api-mddb:
	@if [ ! -e ${d}/api-rpms ]; then \
	    mkdir ${d}/api-rpms; \
	fi; \
	cd ${d}/api-rpms; \
	wget -c http://mirror.centos.org/centos/7/os/x86_64/Packages/basesystem-10.0-7.el7.centos.noarch.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/filesystem-3.2-21.el7.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/httpd-2.4.6-45.el7.centos.x86_64.rpm \
	    http://vault.centos.org/7.2.1511/os/x86_64/Packages/bash-4.2.46-19.el7.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/bash-4.2.46-20.el7_2.x86_64.rpm \
	    http://mirror.centos.org/centos/7/updates/x86_64/Packages/bash-4.2.46-21.el7_3.x86_64.rpm
	docker volume create -d local --name api-test-mddb-volume
	docker run -v api-test-mddb-volume:/mddb:z -v ${d}/api-rpms:/rpms:z,ro --security-opt="label:disable" --rm $(ORG_NAME)/bdcs-import-img


.PHONY: importer mddb api-mddb

import-centos7:
	make weld-f25
	make importer
	mkdir rpms/
	pip install -r tests/requirements.txt

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
	    python ./tests/is_import_busted.py -v $$REPO; \
	done
