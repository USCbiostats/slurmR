build:
	cd ../ && R CMD build sluRm/

check:
	$(MAKE) build && cd .. && R CMD check --as-cran sluRm_*.tar.gz
