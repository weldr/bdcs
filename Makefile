ifdef WORKSPACE
d = ${WORKSPACE}
else
d = ${PWD}
endif

ORG_NAME=weld

importer:
	docker build -t $(ORG_NAME)/build-img -f Dockerfile.build .
	docker create --name build-cont $(ORG_NAME)/build-img
	docker cp build-cont:/root/.cabal/bin/import ./import
	docker rm build-cont
	docker build -t $(ORG_NAME)/import-img .

mddb:
	docker volume create -d local --opt o=size=2GB --name bdcs-mddb-volume
	docker run -v bdcs-mddb-volume:/mddb -v ${d}/rpms:/rpms:z,ro --security-opt="label:disable" --rm $(ORG_NAME)/import-img

api-mddb:
	@if [ ! -e ${d}/api-rpms ]; then \
	    mkdir ${d}/api-rpms; \
	fi; \
	cd ${d}/api-rpms; \
	wget http://mirror.centos.org/centos/7/os/x86_64/Packages/basesystem-10.0-7.el7.centos.noarch.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/filesystem-3.2-21.el7.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/httpd-2.4.6-45.el7.centos.x86_64.rpm \
	    http://mirror.centos.org/centos/7/os/x86_64/Packages/bash-4.2.46-20.el7_2.x86_64.rpm
	docker volume create -d local --name api-test-mddb-volume
	docker run -v api-test-mddb-volume:/mddb:z -v ${d}/api-rpms:/rpms:z,ro --security-opt="label:disable" --rm wiggum/import-img


.PHONY: importer mddb api-mddb
