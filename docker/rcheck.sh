#!/bin/bash
/usr/bin/R CMD check --no-vignettes --no-manual slurmR_*.tar.gz && \
	rm -rf covr || mkdir covr && \
	Rscript -e 'pth <- normalizePath("covr");Sys.setenv(SLURMR_TMP_PATH=pth);saveRDS(covr::package_coverage("slurmR", quiet = FALSE, clean = FALSE, install_path = pth), "covr/dat.rds")' && \
	Rscript -e 'library(covr);readRDS("covr/dat.rds")'
