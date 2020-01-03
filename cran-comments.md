## Test environments

* Local: Ubuntu 18.04.3 LTS ubuntu, R 3.6.2 (2019-12-12)
* Travis: Ubuntu 16.04.6 LTS, R 3.6.1 (2017-01-27)
* Travis: Mac OS X 10.13.6 LTS, R 3.6.2 (2019-12-12)
* USC's HPCC: CentOS Linux 7, R 3.5.0 (2018-04-23)
* AppVeyor: Windows x86_64-w64-mingw32/x64 (64-bit) R 3.6.2 Patched (2019-12-14 r77584)
* win-builder (CRAN): x86_64-w64-mingw32 (64-bit), R 3.6.1 (2019-07-05)
* win-builder (CRAN): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2019-12-18 r77602)
* R-hub builder: Ubuntu 16.04, R-release
* R-hub builder: Fedora Linux, R-release
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* A false positive when checking a URL to a Harvard website.

## Notes

This R package has been tested on Linux systems, and particularly, HPC clusters
working with Slurm. It was tested on USC's HPCC center and others during a review
process of the Journal of Open Source Software. We don't include Slurm as part
of the SystemRequirements field since users without Slurm can still install this
package and use some of its functions.


## On previous CRAN comments

1. Last submission included the note: "Package has help file(s) containing
   install/render-stage \Sexpr{} expressions but no prebuilt PDF manual."
   Evaluation of the Sexpr is now done during the building process. I don't
   see such message now.
   
   I've also added R-hub checks. All is working OK.

Thanks

