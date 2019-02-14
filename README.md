
[![Travis build
status](https://travis-ci.org/USCbiostats/sluRm.svg?branch=master)](https://travis-ci.org/USCbiostats/sluRm)
[![codecov](https://codecov.io/gh/USCbiostats/sluRm/branch/master/graph/badge.svg)](https://codecov.io/gh/USCbiostats/sluRm)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# sluRm

Example

``` r
library(sluRm)
#> On load, `sluRm` sets default options for your jobs (`chdir`, which is the default directory where sluRm will use to create the auxiliar files, and `job-name`, which is the option of the same name in Slurm. You can view/set these at:
#>    ?opts_sluRm
#> or you could just type
#>    "opts_sluRm".

# Suppose that we have 100 vectors of length 50 ~ Unif(0,1)
set.seed(881)
x <- replicate(100, runif(50))
```

We can use the function `Slurm_lapply` to distribute computations

``` r
ans <- Slurm_lapply(x, mean, submit = FALSE)
#> Warning: `X` is not a list. The function will coerce it into one using `as.list`
#> Warning: `submit = FALSE`, which means that the job hasn't been submitted yet. Use sbatch() to submit the job, or you can submit it via command line using the following:
#> sbatch --chdir=/home/vegayon/Documents/sluRm --job-name=sluRm-job-4c891921a977 /home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/01-bash.sh
```

Notice the `submit = FALSE` option. To get more info, we can actually
set the verbose mode on

``` r
opts_sluRm$verbose_on()
ans <- Slurm_lapply(x, mean, submit = FALSE)
#> Warning: `X` is not a list. The function will coerce it into one using `as.list`
#> Warning: The path '/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977' already
#> exists and will be overwritten.
#> 
#> --------------------------------------------------------------------------------
#> `opts_sluRm$get_verbose() == TRUE`. The R script that will be used is located at:
#> /home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/00-rscript.r
#> and has the following contents:
#> --------------------------------------------------------------------------------
#> library(sluRm, lib.loc = "/usr/local/lib/R/site-library")
#> ARRAY_ID         <- as.integer(Slurm_env("SLURM_ARRAY_TASK_ID"))
#> INDICES          <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/INDICES.rds")
#> FUN              <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/FUN.rds")
#> mc.cores         <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/mc.cores.rds")
#> X                <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/X.rds")[INDICES[[ARRAY_ID]]]
#> seeds            <- readRDS("/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/seeds.rds")
#> set.seed(seeds[ARRAY_ID], kind = NULL, normal.kind = NULL)
#> ans <- parallel::mclapply(
#>     X                = X,
#>     FUN              = FUN,
#>     mc.cores         = mc.cores
#> )
#> opts_sluRm$set_chdir("/home/vegayon/Documents/sluRm")
#> opts_sluRm$set_job_name("sluRm-job-4c891921a977", overwrite = FALSE)
#> saveRDS(ans, sluRm::snames("rds", ARRAY_ID), compress=TRUE)
#> 
#> --------------------------------------------------------------------------------
#> The bash file that will be used is located at:
#> /home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/01-bash.sh
#> and has the following contents:
#> --------------------------------------------------------------------------------
#> #!/bin/sh
#> #SBATCH --job-name=sluRm-job-4c891921a977
#> #SBATCH --output=/home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/02-output-%A-%a.out
#> #SBATCH --array=1-2
#> #SBATCH --ntasks=1
#> #SBATCH --cpus-per-task=2
#> export OMP_NUM_THREADS=1
#> /usr/lib/R/bin/Rscript --vanilla /home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/00-rscript.r
#> 
#> --------------------------------------------------------------------------------
#> EOF
#> --------------------------------------------------------------------------------
#> Warning: `submit = FALSE`, which means that the job hasn't been submitted yet. Use sbatch() to submit the job, or you can submit it via command line using the following:
#> sbatch --chdir=/home/vegayon/Documents/sluRm --job-name=sluRm-job-4c891921a977 /home/vegayon/Documents/sluRm/sluRm-job-4c891921a977/01-bash.sh
```

Take a look at the vignette [here](vignettes/getting-started.Rmd).

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("USCbiostats/sluRm")
```

## Example

This is a basic example which shows you how to solve a common problem:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!

## Contributing

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
