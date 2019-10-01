install:
	R CMD INSTALL .

build:
	cd ../ && R CMD build slurmR/

check:
	$(MAKE) build && cd .. && R CMD check --as-cran slurmR_*.tar.gz

clean:
	rm -rf slurmR-job*

docs:
	R -e "roxygen2::roxygenize()" && R -e "pkgdown::build_site()"

man:
	rm slurmR.pdf ; R CMD Rd2pdf --no-preview --output=slurmR.pdf . && evince slurmR.pdf &

covr:
	Rscript -e 'cv <- covr::package_coverage();saveRDS(cv, "slurmR-coverage.rds");covr::codecov(coverage=cv)'

.PHONY: instal build check clean docs man
