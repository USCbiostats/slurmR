install:
	R CMD INSTALL .

build:
	cd ../ && R CMD build sluRm/

check:
	$(MAKE) build && cd .. && R CMD check --as-cran sluRm_*.tar.gz

clean:
	rm -rf sluRm-job*

docs:
	R -e "roxygen2::roxygenize()" && R -e "pkgdown::build_site()"

man:
	rm sluRm.pdf ; R CMD Rd2pdf --no-preview --output=sluRm.pdf . && evince sluRm.pdf &

.PHONY: instal build check clean docs man
