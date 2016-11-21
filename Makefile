importer:
	docker build -t build-img -f Dockerfile.build .
	docker create --name build-cont build-img
	docker cp build-cont:/root/.cabal/bin/import ./import
	docker rm build-cont
	docker build -t import-img .

mddb:
	-rm ${PWD}/mddb/metadata.db
	docker run -v ${PWD}/mddb:/mddb -v ${PWD}/rpms:/rpms:ro --rm -it import-img

.PHONY: importer mddb
