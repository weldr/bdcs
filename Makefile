ifdef WORKSPACE
d = ${WORKSPACE}
else
d = ${CURDIR}
endif

STORE ?= cs.repo
MDDB ?= metadata.db

weld-fedora:
	git clone https://github.com/weldr/welder-deployment
	$(MAKE) -C welder-deployment weld-fedora
	-rm -rf ./welder-deployment

build-and-test: Dockerfile.build
	sudo docker build -t welder/bdcs-build-img -f $< .
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs/ welder/bdcs-build-img

# NOTE: The mddb and content store under ./mddb/ will be removed
#       Unless KEEP_STORE=1 and KEEP_MDDB=1 are set.
mddb:
	sudo docker rm -f mddb-container || true
	sudo docker volume rm bdcs-mddb-volume || true
	sudo docker volume create -d local --name bdcs-mddb-volume
	sudo docker run -v bdcs-mddb-volume:/mddb/ -v ${d}/rpms:/rpms:ro --security-opt="label=disable" \
	    --name mddb-container         \
	    -e "IMPORT_URL=$(IMPORT_URL)" \
	    -e "KEEP_STORE=$(KEEP_STORE)"   \
	    -e "STORE=$(STORE)"             \
	    -e "KEEP_MDDB=$(KEEP_MDDB)"   \
	    -e "MDDB=$(MDDB)"             \
	    welder/bdcs-import-img
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
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/filesystem-3.2-25.el7.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/httpd-2.4.6-80.el7.centos.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/bash-4.2.46-30.el7.x86_64.rpm \
	sudo docker volume create -d local --name api-test-mddb-volume
	sudo docker run -v ${d}/api-mddb:/mddb:z -v ${d}/api-rpms:/rpms:z,ro --security-opt="label:disable" welder/bdcs-import-img


.PHONY: mddb api-mddb ci sandbox tests

ci: build-and-test

ci_after_success:
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs/ \
	    --env "TRAVIS=$$TRAVIS" --env "TRAVIS_JOB_ID=$$TRAVIS_JOB_ID" --entrypoint /usr/bin/make welder/bdcs-build-img coveralls

coveralls: sandbox
	[ -x .cabal-sandbox/bin/hpc-coveralls ] || cabal install hpc-coveralls
	.cabal-sandbox/bin/hpc-coveralls --display-report test-bdcs bdcs

sandbox:
	cabal sandbox init && cabal update && .jenkins/configure-pre-release-dependencies

hlint:
	if [ -z "$$(which hlint)" ]; then \
	    echo "hlint not found in PATH - install it"; \
	    exit 1; \
	else \
	    hlint .; \
	fi

tests: sandbox
	cabal install --dependencies-only --enable-tests --force-reinstall
	cabal configure --enable-tests --enable-coverage --prefix=/usr/local
	cabal build
	cabal test --show-details=always

install:
	cabal install --prefix=/usr/local
