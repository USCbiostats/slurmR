
[![DOI](http://joss.theoj.org/papers/10.21105/joss.01493/status.svg)](https://doi.org/10.21105/joss.01493)
[![Travis build
status](https://travis-ci.org/USCbiostats/slurmR.svg?branch=master)](https://travis-ci.org/USCbiostats/slurmR)
[![codecov](https://codecov.io/gh/USCbiostats/slurmR/branch/master/graph/badge.svg)](https://codecov.io/gh/USCbiostats/slurmR)
[![CRAN
status](https://www.r-pkg.org/badges/version/slurmR)](https://CRAN.R-project.org/package=slurmR)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/slurmR)](https://cran.r-project.org/package=slurmR)
[![status](https://tinyverse.netlify.com/badge/slurmR)](https://CRAN.R-project.org/package=slurmR)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurmR: A Lightweight Wrapper for Slurm <img src="man/figures/logo.png" height="180px" align="right"/>

Slurm Workload Manager is a popular HPC cluster job scheduler found in
many of the top 500 super computers. The `slurmR` R package provides an
R wrapper to it that matches the parallel package’s syntax, this is,
just like `parallel` provides the `parLapply`, `clusterMap`,
`parSapply`, etc., `slurmR` provides `Slurm_lapply`, `Slurm_Map`,
`Slurm_sapply`, etc.

While there are other alternatives such as `future.batchtools`,
`batchtools`, `clustermq`, and `rslurm`, this R package has the
following goals:

1.  It is dependency free, which means that it works out-of-the-box

2.  Puts an emphasis on been similar to the workflow in the R package
    `parallel`

3.  It provides a general framework for the user to create its own
    wrappers without using template files.

4.  Is specialized on Slurm, meaning more flexibility (no need to modify
    template files), and, in the future, better debugging tools
    (e.g. job resubmission).

5.  Provide a backend for the
    [parallel](https://CRAN.R-project.org/view=HighPerformanceComputing)
    package, providing an out-of-the-box method for creating Socket
    cluster objects for multi-node operations. (See the examples below
    on how this can be used with other R packages)

Checkout the [VS section](#vs) section for comparing `slurmR` with other
R packages. Wondering who is using Slurm? Checkout the [list at the end
of this document](#who-uses-slurm).

## Installation

From your HPC command line, you can install the development version from
[GitHub](https://github.com/) with:

``` bash
$ git clone https://github.com/USCbiostats/slurmR.git
$ R CMD INSTALL slurmR/ 
```

The second line assumes you have R available in your system (usually
loaded via `module R` or some other command). Or using the `devtools`
from within R:

``` r
# install.packages("devtools")
devtools::install_github("USCbiostats/slurmR")
```

## Citation

``` 

To cite slurmR in publications use:

  Vega Yon et al., (2019). slurmR: A lightweight wrapper for HPC with
  Slurm. Journal of Open Source Software, 4(39), 1493,
  https://doi.org/10.21105/joss.01493

A BibTeX entry for LaTeX users is

  @Article{,
    title = {slurmR: A lightweight wrapper for HPC with Slurm},
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
library(slurmR)
#  Loading required package: parallel
#  slurmR default option for `tmp_path` (used to store auxiliar files) set to:
#    /home/george/Documents/development/slurmR
#  You can change this and checkout other slurmR options using: ?opts_slurmR, or you could just type "opts_slurmR" on the terminal.

# Suppose that we have 100 vectors of length 50 ~ Unif(0,1)
set.seed(881)
x <- replicate(100, runif(50), simplify = FALSE)
```

We can use the function `Slurm_lapply` to distribute computations

``` r
ans <- Slurm_lapply(x, mean, plan = "none")
#  Warning: [submit = FALSE] The job hasn't been submitted yet. Use sbatch() to submit the job, or you can submit it via command line using the following:
#  sbatch --job-name=slurmr-job-2bf23ae1b6d3 /home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/01-bash.sh
Slurm_clean(ans) # Cleaning after you
```

Notice the `plan = "none"` option, this tells `Slurm_lapply` to only
create the job object, but do nothing with it, i.e., skip submission. To
get more info, we can actually set the verbose mode on

``` r
opts_slurmR$verbose_on()
ans <- Slurm_lapply(x, mean, plan = "none")
#  --------------------------------------------------------------------------------
#  [VERBOSE MODE ON] The R script that will be used is located at: /home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/00-rscript.r and has the following contents:
#  --------------------------------------------------------------------------------
#  .libPaths(c("/home/george/R/x86_64-pc-linux-gnu-library/3.6", "/usr/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library"))
#  message("[slurmR info] Loading variables and functions... ", appendLF = FALSE)
#  Slurm_env <- function (x = "SLURM_ARRAY_TASK_ID") 
#  {
#      y <- Sys.getenv(x)
#      if ((x == "SLURM_ARRAY_TASK_ID") && y == "") {
#          return(1)
#      }
#      y
#  }
#  ARRAY_ID  <- as.integer(Slurm_env("SLURM_ARRAY_TASK_ID"))
#  
#  # The -snames- function creates the write names for I/O of files as a 
#  # function of the ARRAY_ID
#  snames    <- function (type, array_id = NULL, tmp_path = NULL, job_name = NULL) 
#  {
#      if (length(array_id) && length(array_id) > 1) 
#          return(sapply(array_id, snames, type = type, tmp_path = tmp_path, 
#              job_name = job_name))
#      type <- switch(type, r = "00-rscript.r", sh = "01-bash.sh", 
#          out = "02-output-%A-%a.out", rds = if (missing(array_id)) "03-answer-%03i.rds" else sprintf("03-answer-%03i.rds", 
#              array_id), job = "job.rds", stop("Invalid type, the only valid types are `r`, `sh`, `out`, and `rds`.", 
#              call. = FALSE))
#      sprintf("%s/%s/%s", tmp_path, job_name, type)
#  }
#  TMP_PATH  <- "/home/george/Documents/development/slurmR"
#  JOB_NAME  <- "slurmr-job-2bf23ae1b6d3"
#  
#  # The -tcq- function is a wrapper of tryCatch that on error tries to recover
#  # the message and saves the outcome so that slurmR can return OK.
#  tcq <- function (...) 
#  {
#      ans <- tryCatch(..., error = function(e) e)
#      if (inherits(ans, "error")) {
#          ARRAY_ID. <- get("ARRAY_ID", envir = .GlobalEnv)
#          msg <- paste("An error has ocurred while evualting the expression:\n", 
#              paste(deparse(match.call()[[2]]), collapse = "\n"), 
#              "\n in ", "ARRAY_ID # ", ARRAY_ID.)
#          warning(msg, immediate. = TRUE, call. = FALSE)
#          ans$message <- paste(ans$message, msg)
#          saveRDS(ans, snames("rds", tmp_path = get("TMP_PATH", 
#              envir = .GlobalEnv), job_name = get("JOB_NAME", envir = .GlobalEnv), 
#              array_id = ARRAY_ID.))
#          q("no")
#      }
#      invisible(ans)
#  }
#  message("done loading variables and functions.")
#  tcq({
#    INDICES <- readRDS("/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/INDICES.rds")
#  })
#  tcq({
#    X <- readRDS(sprintf("/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/X_%04d.rds", ARRAY_ID))
#  })
#  tcq({
#    FUN <- readRDS("/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/FUN.rds")
#  })
#  tcq({
#    mc.cores <- readRDS("/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/mc.cores.rds")
#  })
#  tcq({
#    seeds <- readRDS("/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/seeds.rds")
#  })
#  set.seed(seeds[ARRAY_ID], kind = NULL, normal.kind = NULL)
#  tcq({
#    ans <- parallel::mclapply(
#      X                = X,
#      FUN              = FUN,
#      mc.cores         = mc.cores
#  )
#  })
#  saveRDS(ans, sprintf("/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/03-answer-%03i.rds", ARRAY_ID), compress = TRUE)
#  --------------------------------------------------------------------------------
#  The bash file that will be used is located at: /home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/01-bash.sh and has the following contents:
#  --------------------------------------------------------------------------------
#  #!/bin/sh
#  #SBATCH --job-name=slurmr-job-2bf23ae1b6d3
#  #SBATCH --output=/home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/02-output-%A-%a.out
#  #SBATCH --array=1-2
#  #SBATCH --job-name=slurmr-job-2bf23ae1b6d3
#  #SBATCH --cpus-per-task=1
#  #SBATCH --ntasks=1
#  export OMP_NUM_THREADS=1
#  /usr/lib/R/bin/Rscript  /home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/00-rscript.r
#  --------------------------------------------------------------------------------
#  EOF
#  --------------------------------------------------------------------------------
#  Warning: [submit = FALSE] The job hasn't been submitted yet. Use sbatch() to submit the job, or you can submit it via command line using the following:
#  sbatch --job-name=slurmr-job-2bf23ae1b6d3 /home/george/Documents/development/slurmR/slurmr-job-2bf23ae1b6d3/01-bash.sh
Slurm_clean(ans) # Cleaning after you
```

## Example 2: Job resubmission

The following example from the package’s manual.

``` r
# Submitting a simple job
job <- Slurm_EvalQ(slurmR::WhoAmI(), njobs = 20, plan = "submit")

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

## Example 3: Using slurmR and future/doParallel/boot/…

The function `makeSlurmCluster` creates a PSOCK cluster within a Slurm
HPC network, meaning that users can go beyond a single node cluster
object and take advantage of Slurm to create a multi-node cluster
object. This feature allows then using `slurmR` with other R packages
that support working with `SOCKcluster` class objects. Here are some
examples

With the [`future`](https://cran.r-project.org/package=future) package

``` r
library(future)
library(slurmR)

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
library(slurmR)

cl <- makeSlurmCluster(50)

registerDoParallel(cl)
m <- matrix(rnorm(9), 3, 3)
foreach(i=1:nrow(m), .combine=rbind) 

stopCluster(cl)
```

## Example 4: Using slurmR directly from the command line

The `slurmR` package has a couple of convenient functions designed for
the user to save time. First, the function `sourceSlurm()` allows
skipping the explicit creating of a bash script file to be used together
with `sbatch` by putting all the required config files on the first
lines of an R scripts, for example:

    #!/bin/sh
    #SBATCH --account=lc_ggv
    #SBATCH --partition=scavenge
    #SBATCH --time=01:00:00
    #SBATCH --mem-per-cpu=4G
    #SBATCH --job-name=Waiting
    Sys.sleep(10)
    message("done.")

Is an R script that on the first line coincides with that of a bash
script for Slurm: `#!/bin/bash`. The following lines start with
`#SBATCH` explicitly specifying options for `sbatch`, and the reminder
lines are just R code.

The previous R script is included in the package (type
`system.file("example.R", package="slurmR")`).

Imagine that that R script is named `example.R`, then you use the
`sourceSlurm` function to submit it to Slurm as follows:

``` r
slurmR::sourceSlurm("example.R")
```

This will create the corresponding bash file required to be used with
`sbatch`, and submit it to Slurm.

Another nice tool is the `slurmr_cmd()`. This function will create a
simple bash script that can be used as a command line tool to submit
this type of R scripts. Moreover, this command will can add the command
to your session’s
[**alias**](https://en.wikipedia.org/wiki/Alias_\(command\)) as follows:

``` r
library(slurmR)
slurmr_cmd("~", add_alias = TRUE)
```

Once that’s done, you can simply submit R scripts with “Slurm-like
headers” (as shown previously) as follows:

``` bash
$ slurmr example.R
```

## VS

There are several ways to enhance R for HPC. Depending on what are your
goals/restrictions/preferences, you can use any of the following from
this **manually curated** list:

| Package                                                                       | Rerun (1) | \*apply (2) | makeCluster (3) | Slurm options | Dependencies                                                                                                             | Activity                                                                                                                                         |
| :---------------------------------------------------------------------------- | :-------- | :---------- | :-------------- | :------------ | :----------------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------- |
| [**slurmR**](https://cran.r-project.org/package=slurmR)                       | yes       | yes         | yes             | on the fly    | [![status](https://tinyverse.netlify.com/badge/slurmR)](https://CRAN.R-project.org/package=slurmR)                       | [![Activity](https://img.shields.io/github/last-commit/USCbiostats/slurmR)](https://github.com/USCbiostats/slurmR)                               |
| [**drake**](https://cran.r-project.org/package=drake)                         | yes       | \-          | \-              | by template   | [![status](https://tinyverse.netlify.com/badge/drake)](https://CRAN.R-project.org/package=drake)                         | [![Activity](https://img.shields.io/github/last-commit/ropensci/drake)](https://github.com/ropensci/drake)                                       |
| [**rslurm**](https://cran.r-project.org/package=rslurm)                       | \-        | yes         | \-              | on the fly    | [![status](https://tinyverse.netlify.com/badge/rslurm)](https://CRAN.R-project.org/package=rslurm)                       | [![Activity](https://img.shields.io/github/last-commit/SESYNC-ci/rslurm)](https://github.com/SESYNC-ci/rslurm)                                   |
| [**future.batchtools**](https://cran.r-project.org/package=future.batchtools) | \-        | yes         | yes             | by template   | [![status](https://tinyverse.netlify.com/badge/future.batchtools)](https://CRAN.R-project.org/package=future.batchtools) | [![Activity](https://img.shields.io/github/last-commit/HenrikBengtsson/future.batchtools)](https://github.com/HenrikBengtsson/future.batchtools) |
| [**batchtools**](https://cran.r-project.org/package=batchtools)               | yes       | yes         | \-              | by template   | [![status](https://tinyverse.netlify.com/badge/batchtools)](https://CRAN.R-project.org/package=batchtools)               | [![Activity](https://img.shields.io/github/last-commit/mllg/batchtools)](https://github.com/mllg/batchtools)                                     |
| [**clustermq**](https://cran.r-project.org/package=clustermq)                 | \-        | \-          | \-              | by template   | [![status](https://tinyverse.netlify.com/badge/clustermq)](https://CRAN.R-project.org/package=clustermq)                 | [![Activity](https://img.shields.io/github/last-commit/mschubert/clustermq)](https://github.com/mschubert/clustermq)                             |

1)  After errors, the part or the entire job can be resubmitted.
2)  Functionality similar to the apply family in base R, e.g. lapply,
    sapply, mapply or similar.
3)  Creating a cluster object using either MPI or Socket connection.

The packages [**slurmR**](https://cran.r-project.org/package=slurmR),
[**rslurm**](https://cran.r-project.org/package=rslurm) work only on
Slurm. The [**drake**](https://cran.r-project.org/package=drake) package
is focused on workflows.

## Contributing

We welcome contributions to `slurmR`. Whether it is reporting a bug,
starting a discussion by asking a question, or proposing/requesting a
new feature, please go by creating a new issue
[here](https://github.com/USCbiostats/slurmR/issues) so that we can talk
about it.

Please note that this project is released with a Contributor Code of
Conduct (see the CODE\_OF\_CONDUCT.md file included in this project). By
participating in this project you agree to abide by its terms.

## Who uses Slurm

Here is a manually curated list of institutions using Slurm:

| Institution                                            | Country | Link                                                                                                         |
| ------------------------------------------------------ | ------- | ------------------------------------------------------------------------------------------------------------ |
| USC High Performance Computing Center                  | US      | [link](https://hpcc.usc.edu)                                                                                 |
| Princeton Research Computing                           | US      | [link](https://researchcomputing.princeton.edu/education/online-tutorials/getting-started/introducing-slurm) |
| Harvard FAS                                            | US      | [link](https://www.rc.fas.harvard.edu/resources/quickstart-guide/)                                           |
| Harvard HMS research computing                         | US      | [link](https://rc.hms.harvard.edu/)                                                                          |
| UCSan Diego WM Keck Lab for Integrated Biology         | US      | [link](https://keck2.ucsd.edu/dokuwiki/doku.php/wiki:slurm)                                                  |
| Stanford Sherlock                                      | US      | [link](https://www.sherlock.stanford.edu/docs/overview/introduction/)                                        |
| Stanford SCG Informatics Cluster                       | US      | [link](https://login.scg.stanford.edu/tutorials/job_scripts/)                                                |
| Berkeley Research IT                                   | US      | [link](http://research-it.berkeley.edu/services/high-performance-computing/running-your-jobs)                |
| University of Utah CHPC                                | US      | [link](https://www.chpc.utah.edu/documentation/software/slurm.php)                                           |
| University of Michigan Biostatistics cluster           | US      | [link](https://sph.umich.edu/biostat/computing/cluster/slurm.html)                                           |
| The University of Kansas Center for Research Computing | US      | [link](https://crc.ku.edu/hpc/how-to)                                                                        |
| University of Cambridge MRC Biostatistics Unit         | UK      | [link](https://www.mrc-bsu.cam.ac.uk/research-and-development/high-performance-computing-at-the-bsu/)        |
| Indiana University                                     | US      | [link](https://kb.iu.edu/d/awrz)                                                                             |
| Caltech HPC Center                                     | US      | [link](https://www.hpc.caltech.edu/documentation/slurm-commands)                                             |
| Institute for Advanced Study                           | US      | [link](https://www.sns.ias.edu/computing/slurm)                                                              |
| UTSouthwestern Medical Center BioHPC                   | US      | [link](https://portal.biohpc.swmed.edu/content/guides/slurm/)                                                |
| Vanderbilt University ACCRE                            | US      | [link](https://www.vanderbilt.edu/accre/documentation/slurm/)                                                |
| University of Virginia Research Computing              | US      | [link](https://www.rc.virginia.edu/userinfo/rivanna/slurm/)                                                  |
| Center for Advanced Computing                          | CA      | [link](https://cac.queensu.ca/wiki/index.php/SLURM)                                                          |
| SciNet                                                 | CA      | [link](https://docs.scinet.utoronto.ca/index.php/Slurm)                                                      |
| NLHPC                                                  | CL      | [link](http://usuarios.nlhpc.cl/index.php/SLURM)                                                             |
| Kultrun                                                | CL      | [link](http://www.astro.udec.cl/kultrun)                                                                     |
| Matbio                                                 | CL      | [link](http://www.matbio.cl/cluster/)                                                                        |
| TIG MIT                                                | US      | [link](https://tig.csail.mit.edu/shared-computing/slurm/)                                                    |
| MIT Supercloud                                         | US      | [link](https://supercloud.mit.edu/submitting-jobs)                                                           |
| Oxford’s ARC                                           | UK      | [link](https://help.it.ox.ac.uk/arc/job-scheduling)                                                          |

## Funding

Supported by National Cancer Institute Grant \#1P01CA196596.

Computation for the work described in this paper was supported by the
University of Southern California’s Center for High-Performance
Computing (hpcc.usc.edu).
