ifdef WORKSPACE
d = ${WORKSPACE}
else
d = ${PWD}
endif

importer:
	docker build -t wiggum/build-img -f Dockerfile.build .
	docker create --name build-cont wiggum/build-img
	docker cp build-cont:/root/.cabal/bin/import ./import
	docker rm build-cont
	docker build -t wiggum/import-img .

mddb:
	docker volume create -d local --opt o=size=2GB --name bdcs-mddb-volume
	docker run -v bdcs-mddb-volume:/mddb -v ${d}/rpms:/rpms:z,ro --rm wiggum/import-img

.PHONY: importer mddb
