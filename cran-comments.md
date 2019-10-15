## Test environments

* local ubuntu, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1
* centos within a HPC cluster using Slurm R 3.6.0

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

1. The "invalid URLs: URL: https://rc.hms.harvard.edu/#cluster" has been corrected.
   
2. On the package's name. I've contacted the team behind Slurm and, unfortunately,
   they asked me to change the name. Because of this, the R package is now called
   slurmR. Besides of the name, all documentation has been updated to reflect this
   change.

Thanks

