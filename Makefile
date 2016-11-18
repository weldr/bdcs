importer:
	sudo docker build -t build-img -f Dockerfile.build .
	sudo docker create --name build-cont build-img
	sudo docker cp build-cont:/root/.cabal/bin/import ./import
	sudo docker rm build-cont
	sudo docker build -t import-img .

mddb:
	sudo docker run -v mddb:/mddb -v rpms:/rpms:ro --rm -it import-img
