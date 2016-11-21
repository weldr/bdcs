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
	-rm ${d}/mddb/metadata.db
	docker run -v ${d}/mddb:/mddb -v ${d}/rpms:/rpms:ro --rm -it import-img

.PHONY: importer mddb
