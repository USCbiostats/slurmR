.PHONY: install build check clean docs man checkalloc

slurmR.tar.gz: R/* DESCRIPTION inst/tinytest/* tests/* 
	R CMD build . ; rm slurmR.tar.gz ; mv slurmR*.tar.gz slurmR.tar.gz

install: clean slurmR.tar.gz
	R CMD REMOVE slurmR ; R CMD INSTALL --preclean slurmR.tar.gz

checkalloc:
	salloc --partition=scavenge --time=01:00:00 --cpus-per-task=4 --job-name=slurmR-pkg-check --mem-per-cpu=1G srun -n1 $(MAKE) check

check: clean install
	R CMD check --as-cran slurmR.tar.gz ; $(MAKE) clean

checknotest:
	$(MAKE) build && cd .. && R CMD check --as-cran --no-tests slurmR_*.tar.gz

clean:
	rm -rf slurmr-job*; rm -rf slurm*.out

docs: man/slurmR.Rd README.md docs/index.html

man/slurmR.Rd: R/*.R
	R -e "roxygen2::roxygenize()"

README.md: README.Rmd
	R -e "rmarkdown::render('README.Rmd')"

docs/index.html: man/slurmR.Rd README.md
	R -e "pkgdown::build_site()"

man:
	rm slurmR.pdf ; R CMD Rd2pdf --no-preview --output=slurmR.pdf . && \
	  evince slurmR.pdf &

covr: 
	Rscript -e 'cv <- covr::package_coverage();saveRDS(cv, "slurmR-coverage.rds");covr::codecov(coverage=cv)' \
		> slurmR-coverage.Rout &


