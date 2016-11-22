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
	if [ -f ${d}/mddb/metadata.db ]; then rm ${d}/mddb/metadata.db; fi
	docker run -v ${d}/mddb:/mddb -v ${d}/rpms:/rpms:ro --rm import-img

.PHONY: importer mddb
