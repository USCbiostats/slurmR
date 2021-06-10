build:
	cd ../ && $(MAKE) slurmR_* && cp slurmR_* docker/. && \
	docker build -t uscbiostats/slurmr:interactive -f docker/Dockerfile.interactive docker/ && \
	docker build -t uscbiostats/slurmr:rcmdcheck -f docker/Dockerfile.rcmdcheck docker/ 
push:
	docker push uscbiostats/slurmr:interactive && \
		docker push uscbiostats/slurmr:rcmdcheck
interactive:
	docker run --detach --publish 10022:22 uscbiostats/slurmr:interactive && \
		echo "To login type\n\tssh -i id_rsa xenon@localhost -p 10022"
check:
	rm -rf ../slurmR-docker.Rcheck && \
		docker run -i --rm --publish 10022:22 -v$(PWD)/..:/home/xenon/slurmR \
		-w/home/xenon uscbiostats/slurmr:rcmdcheck
login:
	ssh -p 10022 -i id_rsa xenon@localhost
