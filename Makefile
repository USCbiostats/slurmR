install:
	R CMD INSTALL .

build:
	cd ../ && R CMD build sluRm/

check:
	$(MAKE) build && cd .. && R CMD check --as-cran sluRm_*.tar.gz

clean:
	rm -rf sluRm-job*
