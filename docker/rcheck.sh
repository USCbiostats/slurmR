#!/bin/bash
/usr/bin/R CMD check --no-vignettes --no-manual slurmR_*.tar.gz && \
	rm -rf covr || mkdir covr && \
	Rscript -e 'pth <- "/home/slurmr-test/"; Sys.setenv(SLURMR_TMP_PATH=pth);saveRDS(covr::package_coverage("slurmR", quiet = FALSE, clean = FALSE, install_path = pth), "/home/slurmr-test/covr.rds")' && \
	Rscript -e 'library(covr); x <- readRDS("/home/slurmr-test/covr.rds"); codecov(coverage=x, token = Sys.getenv("CODECOV_TOKEN"))'
