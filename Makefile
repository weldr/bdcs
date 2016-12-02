ifdef WORKSPACE
d = ${WORKSPACE}
else
d = ${PWD}
endif

importer:
	docker build -t build-img -f Dockerfile.build .
	docker create --name build-cont build-img
	docker cp build-cont:/root/.cabal/bin/import ./import
	docker rm build-cont
	docker build -t import-img .

mddb:
	docker volume create -d local --opt o=size=2GB --name bdcs-mddb-volume
	docker run -v bdcs-mddb-volume:/mddb -v ${d}/rpms:/rpms:ro --rm import-img

.PHONY: importer mddb
