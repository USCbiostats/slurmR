## Test environments

* local ubuntu, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

This R package has been tested on Linux systems, and particularly, HPC clusters
working with Slurm. It was tested on USC's HPCC center and others during a review
process of the Journal of Open Source Software. We don't include Slurm as part
of the SystemRequirements field since users without Slurm can still install this
package and use some of its functions.

## On the latest CRAN comments

1. The "invalid URLs: URL: https://rc.hms.harvard.edu/#cluster" is a false positive.
   either way, it is not the main website of Slurm but the site of one of the clusters
   that I've listed on the README.md file. No need to change that.
   
2. The Description field does not start with the name of the package, but rather
   with the name of the HPC job scheduler's name. So this is a false positive 
   as well. Notice that the name of the package is "sluRm", while the scheduler
   is "Slurm" (different capitals :P)

Thanks

