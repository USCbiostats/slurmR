build:
	cd ../ && $(MAKE) slurmR_* && cp slurmR_* docker/. && \
	docker build -t uscbiostats/slurmr-dev:latest docker/
push:
	docker push uscbiostats/slurmr-dev:latest
