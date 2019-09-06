
[![DOI](http://joss.theoj.org/papers/10.21105/joss.01493/status.svg)](https://doi.org/10.21105/joss.01493)
[![Travis build
status](https://travis-ci.org/USCbiostats/sluRm.svg?branch=master)](https://travis-ci.org/USCbiostats/sluRm)
[![codecov](https://codecov.io/gh/USCbiostats/sluRm/branch/master/graph/badge.svg)](https://codecov.io/gh/USCbiostats/sluRm)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/sluRm)](https://CRAN.R-project.org/package=sluRm)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# sluRm: A Lightweight Wrapper for Slurm <img src="man/figures/logo.png" height="180px" align="right"/>

Slurm Workload Manager is a popular HPC cluster job scheduler found in
many of the top 500 super computers. The `sluRm` R package provides an R
wrapper to it that matches the parallel package’s syntax, this is, just
like `parallel` provides the `parLapply`, `clusterMap`, `parSapply`,
etc., `sluRm` provides `Slurm_lapply`, `Slurm_Map`, `Slurm_sapply`, etc.

While there are other alternatives such as `future.batchtools`,
`batchtools`, `clustermq`, and `rslurm`, this R package has the
following goals:

1.  It is dependency free, which means that it works out-of-the-box

2.  Puts an emphasis on been similar to the workflow in the R package
    `parallel`

3.  It provides a general framework for the user to create its own
    wrappers without using template files.

4.  Is specialized on Slurm, meaning more flexibility (no need to modify
    template files), and, in the future, better debuging tools (e.g. job
    resubmission).

5.  Provide a backend for the
    [parallel](https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf)
    package, providing an out-of-the-box method for creating Socket
    cluster objects for multi-node operations. (See the examples below
    on how this can be used with other R packages)

Checkout the [VS section](#vs) section for comparing `sluRm` with other
R packages. Wondering who is using Slurm? Checkout the [list at the end
of this document](#who-uses-slurm).

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("USCbiostats/sluRm")
```

## Citation

``` 

To cite sluRm in publications use:

  Vega Yon et al., (2019). sluRm: A lightweight wrapper for HPC with
  Slurm. Journal of Open Source Software, 4(39), 1493,
  https://doi.org/10.21105/joss.01493

A BibTeX entry for LaTeX users is

  @Article{,
    title = {sluRm: A lightweight wrapper for HPC with Slurm},
    author = {George {Vega Yon} and Paul Marjoram},
    journal = {The Journal of Open Source Software},
    year = {2019},
    month = {jul},
    volume = {4},
    number = {39},
    doi = {10.21105/joss.01493},
    url = {https://doi.org/10.21105/joss.01493},
  }
```

## Example 1: Computing means (and looking under the hood)

``` r
library(sluRm)
#  Loading required package: parallel
#  On load, `sluRm` sets default options for your jobs (`tmp_path`, which is the default directory where sluRm will use to create the auxiliar files, and `job-name`, which is the option of the same name in Slurm. You can view/set these at:
#     ?opts_sluRm
#  or you could just type
#     "opts_sluRm".

# Suppose that we have 100 vectors of length 50 ~ Unif(0,1)
set.seed(881)
x <- replicate(100, runif(50), simplify = FALSE)
```

We can use the function `Slurm_lapply` to distribute computations

``` r
ans <- Slurm_lapply(x, mean, plan = "none")
#  Warning: [submit = FALSE] The job hasn't been submitted yet. Use sbatch() to submit the job, or you can submit it via command line using the following:
#  sbatch --job-name=sluRm-job-1f9616c9bd21 /home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/01-bash.sh
Slurm_clean(ans) # Cleaning after you
```

Notice the `plan = "none"` option, this tells `Slurm_lapply` to only
create the job object, but do nothing with it, i.e., skip submission. To
get more info, we can actually set the verbose mode on

``` r
opts_sluRm$verbose_on()
ans <- Slurm_lapply(x, mean, plan = "none")
#  
#  --------------------------------------------------------------------------------
#  [VERBOSE MODE ON] The R script that will be used is located at:
#  /home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/00-rscript.r
#  and has the following contents:
#  --------------------------------------------------------------------------------
#  .libPaths(c("/usr/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library"))
#  Slurm_env <- function (x) 
#  {
#      y <- Sys.getenv(x)
#      if ((x == "SLURM_ARRAY_TASK_ID") && y == "") {
#          return(1)
#      }
#      y
#  }
#  ARRAY_ID         <- as.integer(Slurm_env("SLURM_ARRAY_TASK_ID"))
#  JOB_PATH         <- "/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/"
#  INDICES          <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/INDICES.rds")
#  X                <- readRDS(sprintf("/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/X_%04d.rds", ARRAY_ID))
#  FUN              <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/FUN.rds")
#  mc.cores         <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/mc.cores.rds")
#  seeds            <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/seeds.rds")
#  set.seed(seeds[ARRAY_ID], kind = NULL, normal.kind = NULL)
#  ans <- parallel::mclapply(
#      X                = X,
#      FUN              = FUN,
#      mc.cores         = mc.cores
#  )
#  saveRDS(ans, sprintf("/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/03-answer-%03i.rds", ARRAY_ID), compress = TRUE)
#  
#  --------------------------------------------------------------------------------
#  The bash file that will be used is located at:
#  /home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/01-bash.sh
#  and has the following contents:
#  --------------------------------------------------------------------------------
#  #!/bin/sh
#  #SBATCH --job-name=sluRm-job-1f9616c9bd21
#  #SBATCH --output=/home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/02-output-%A-%a.out
#  #SBATCH --array=1-2
#  #SBATCH --ntasks=1
#  #SBATCH --cpus-per-task=1
#  export OMP_NUM_THREADS=1
#  /usr/lib/R/bin/Rscript --vanilla /home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/00-rscript.r
#  
#  --------------------------------------------------------------------------------
#  EOF
#  --------------------------------------------------------------------------------
#  Warning: [submit = FALSE] The job hasn't been submitted yet. Use sbatch() to submit the job, or you can submit it via command line using the following:
#  sbatch --job-name=sluRm-job-1f9616c9bd21 /home/vegayon/Documents/sluRm/sluRm-job-1f9616c9bd21/01-bash.sh
Slurm_clean(ans) # Cleaning after you
```

## Example 2: Job resubmission

The following example from the package’s manual.

``` r
# Submitting a simple job
job <- Slurm_EvalQ(sluRm::WhoAmI(), njobs = 20, plan = "submit")

# Checking the status of the job (we can simply print)
job
status(job) # or use the state function
sacct(job) # or get more info with the sactt wrapper.

# Suppose some of the jobs are taking too long to complete (say 1, 2, and 15 through 20)
# we can stop it and resubmit the job as follows:
scancel(job)

# Resubmitting only 
sbatch(job, array = "1,2,15-20") # A new jobid will be assigned

# Once its done, we can collect all the results at once
res <- Slurm_collect(job)

# And clean up if we don't need to use it again
Slurm_clean(res)
```

Take a look at the vignette [here](vignettes/getting-started.Rmd).

## Example 3: Using sluRm and future/doParallel/boot/…

The function `makeSlurmCluster` creates a PSOCK cluster within a Slurm
HPC network, meaning that users can go beyond a single node cluster
object and take advantage of Slurm to create a multi-node cluster
object. This feature allows then using `sluRm` with other R packages
that support working with `SOCKcluster` class objects. Here are some
examples

With the [`future`](https://cran.r-project.org/package=future) package

``` r
library(future)
library(sluRm)

cl <- makeSlurmCluster(50)

# It only takes using a cluster plan!
plan(cluster, cl)

...your fancy futuristic code...

# Slurm Clusters are stopped in the same way any cluster object is
stopCluster(cl)
```

With the [`doParallel`](https://cran.r-project.org/package=doParallel)
package

``` r
library(doParallel)
library(sluRm)

cl <- makeSlurmCluster(50)

registerDoParallel(cl)
m <- matrix(rnorm(9), 3, 3)
foreach(i=1:nrow(m), .combine=rbind) 

stopCluster(cl)
```

## VS

There are several ways to enhance R for HPC. Depending on what are your
goals/restrictions/preferences, you can use any of the following:

<table cellspacing="0" border="0">

<colgroup width="125">

</colgroup>

<colgroup width="85">

</colgroup>

<colgroup width="73">

</colgroup>

<colgroup span="3" width="85">

</colgroup>

<colgroup width="125">

</colgroup>

<colgroup width="104">

</colgroup>

<tbody>

<tr>

<td height="36" align="center" valign="middle" bgcolor="#FFFFFF">

<b>Package</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>Rerun (1)</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>apply family (2)</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>Slurm options</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>Focus on \[blank\]</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>System \[blank\]</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>Dependencies
(3)</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

<b>Status</b>

</td>

</tr>

<tr>

<td style="border-top: 1px solid #000000" height="36" align="left" valign="middle" bgcolor="#FFFFFF">

<b>drake</b>

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

yes

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

no

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

by
template

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

workflows

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

agnostic

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF" sdnum="1033;0;@">

5/9

</td>

<td style="border-top: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

active

</td>

</tr>

<tr>

<td height="36" align="left" valign="middle" bgcolor="#CCCCCC">

<b>sluRm</b>

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC">

yes

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC">

yes

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC">

on the fly

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC">

calls

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC">

specific

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC" sdnum="1033;0;@">

0/0

</td>

<td align="center" valign="middle" bgcolor="#CCCCCC">

active

</td>

</tr>

<tr>

<td height="36" align="left" valign="middle" bgcolor="#FFFFFF">

<b>rslurm</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

no

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

no

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

on the fly

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

calls

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

specific

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF" sdnum="1033;0;@">

1/1

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

inactive (since 2017)

</td>

</tr>

<tr>

<td height="36" align="left" valign="middle" bgcolor="#FFFFFF">

<b>future.batchtools</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

no

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

yes

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

by template

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

calls

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

agnostic

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF" sdnum="1033;0;@">

2/24

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

active

</td>

</tr>

<tr>

<td height="36" align="left" valign="middle" bgcolor="#FFFFFF">

<b>batchtools</b>

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

yes

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

yes

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

by
template

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

calls

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

agnostic

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF" sdnum="1033;0;@">

12/20

</td>

<td align="center" valign="middle" bgcolor="#FFFFFF">

active

</td>

</tr>

<tr>

<td style="border-bottom: 1px solid #000000" height="36" align="left" valign="middle" bgcolor="#FFFFFF">

<b>clustermq</b>

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

no

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

no

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

by
template

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

calls

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

agnostic

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF" sdnum="1033;0;@">

6/16

</td>

<td style="border-bottom: 1px solid #000000" align="center" valign="middle" bgcolor="#FFFFFF">

active

</td>

</tr>

<tr>

<td colspan="8" height="17" align="left" valign="middle" bgcolor="#FFFFFF">

\[1\] After errors, the part or the entire job can be
resubmitted.

</td>

</tr>

<tr>

<td colspan="8" height="17" align="left" valign="middle" bgcolor="#FFFFFF">

\[2\] Functionality similar to the apply family in base
R.

</td>

</tr>

<tr>

<td colspan="8" height="17" align="left" valign="middle" bgcolor="#FFFFFF">

\[3\] Number of directed/recursive dependencies. As reported in
<a href="https://tinyverse.netlify.com/status/">https://tinyverse.netlify.com/status/</a>
(June 4th, 2019)

</td>

</tr>

</tbody>

</table>

## Contributing

We welcome contributions to `sluRm`. Whether it is reporting a bug,
starting a discussion by asking a question, or proposing/requesting a
new feature, please go by creating a new issue
[here](https://github.com/USCbiostats/sluRm/issues) so that we can talk
about it.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## Who uses Slurm

Here is a list of educational institutions using Slurm:

  - USC [HPCC](https://hpcc.usc.edu)

  - Princeton [Princeton Research
    Computing](https://researchcomputing.princeton.edu/education/online-tutorials/getting-started/introducing-slurm)

  - Harvard
    [FAS](https://www.rc.fas.harvard.edu/resources/quickstart-guide/),
    [HMS research computing](https://rc.hms.harvard.edu/#cluster)

  - UCSan Diego [WM Keck Lab for Integrated
    Biology](https://keck2.ucsd.edu/dokuwiki/doku.php/wiki:slurm)

  - Stanford
    [Sherlock](https://www.sherlock.stanford.edu/docs/overview/introduction/),
    [SCG Informatics
    Cluster](https://login.scg.stanford.edu/tutorials/job_scripts/)

  - Berkeley [Research
    IT](http://research-it.berkeley.edu/services/high-performance-computing/running-your-jobs)

  - University of Utah
    [CHPC](https://www.chpc.utah.edu/documentation/software/slurm.php)

## Funding

Supported by National Cancer Institute Grant \#1P01CA196596.

Computation for the work described in this paper was supported by the
University of Southern California’s Center for High-Performance
Computing (hpcc.usc.edu).
