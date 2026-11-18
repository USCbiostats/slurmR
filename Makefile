VERSION:=$(shell Rscript -e 'x<-readLines("DESCRIPTION");cat(gsub(".+[:]\\s*", "", x[grepl("^Vers", x)]))')
.PHONY: install build check clean docs man checkalloc

slurmR_$(VERSION).tar.gz: R/* DESCRIPTION inst/tinytest/* tests/* NAMESPACE
	R CMD build . 

install: 
	Rscript -e 'devtools::install()'

checkalloc: install clean
	salloc --time=01:00:00 --cpus-per-task=4 --job-name=slurmR-pkg-check --mem-per-cpu=1G srun -n1 $(MAKE) check

check:
	Rscript -e 'devtools::check()'

checknotest: clean slurmR_$(VERSION).tar.gz
	R CMD check --as-cran --no-tests slurmR_$(VERSION).tar.gz

clean:
	rm -rf slurmr-job*; rm -rf slurm*.out; rm -rf docker/slurmR.Rcheck slurm*.Rcheck ; \
		rm -f vignettes/*.R vignettes/*.html; \
		rm -f slurmR_$(VERSION).tar.gz

docs: NAMESPACE README.md

NAMESPACE:
	Rscript -e 'devtools::document()'

README.md: README.qmd
	quarto render README.qmd

docs/index.html: NAMESPACE README.md
	R -e "pkgdown::build_site()"

covr: 
	rm -rf covr || mkdir covr && \
		Rscript -e 'pth <- normalizePath("~/slurmR/covr");Sys.setenv(SLURMR_TMP_PATH=pth);saveRDS(covr::package_coverage(quiet = FALSE, clean = FALSE, install_path = pth), "covr/dat.rds")' && \
		echo "Now you can go ahead and upload the coverage"

inst/NEWS: NEWS.md
	Rscript -e "rmarkdown::pandoc_convert('NEWS.md', 'plain', output='inst/NEWS')" && \
		head -n 80 inst/NEWS



