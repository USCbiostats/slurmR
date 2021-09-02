VERSION:=$(shell Rscript -e 'x<-readLines("DESCRIPTION");cat(gsub(".+[:]\\s*", "", x[grepl("^Vers", x)]))')
.PHONY: install build check clean docs man checkalloc

slurmR_$(VERSION).tar.gz: R/* DESCRIPTION inst/tinytest/* tests/* NAMESPACE
	R CMD build . 

install: slurmR_$(VERSION).tar.gz
	R CMD REMOVE slurmR ; R CMD INSTALL --preclean slurmR_$(VERSION).tar.gz

checkalloc: install clean
	salloc --time=01:00:00 --cpus-per-task=4 --job-name=slurmR-pkg-check --mem-per-cpu=1G srun -n1 $(MAKE) check

check: slurmR_$(VERSION).tar.gz clean
	R CMD check --as-cran slurmR_$(VERSION).tar.gz

checknotest: clean slurmR_$(VERSION).tar.gz
	R CMD check --as-cran --no-tests slurmR_$(VERSION).tar.gz

clean:
	rm -rf slurmr-job*; rm -rf slurm*.out; rm -rf docker/slurmR.Rcheck slurm*.Rcheck ; \
		rm -f vignettes/*.R vignettes/*.html

docs: NAMESPACE README.md

NAMESPACE: R/*.R
	R -e "roxygen2::roxygenize()"

README.md: README.Rmd
	R -e "rmarkdown::render('README.Rmd')"

docs/index.html: NAMESPACE README.md
	R -e "pkgdown::build_site()"

man:
	rm slurmR.pdf ; R CMD Rd2pdf --no-preview --output=slurmR.pdf . && \
	  evince slurmR.pdf &

covr: 
	rm -rf covr || mkdir covr && \
		Rscript -e 'pth <- normalizePath("~/slurmR/covr");Sys.setenv(SLURMR_TMP_PATH=pth);saveRDS(covr::package_coverage(quiet = FALSE, clean = FALSE, install_path = pth), "covr/dat.rds")' && \
		echo "Now you can go ahead and upload the coverage"

inst/NEWS: NEWS.md
	Rscript -e "rmarkdown::pandoc_convert('NEWS.md', 'plain', output='inst/NEWS')" && \
		head -n 80 inst/NEWS



