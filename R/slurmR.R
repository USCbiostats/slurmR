#' A Lightweight Wrapper for 'Slurm'
#' @details
#' To cite slurmR in publications use:
#'
#' Vega Yon et al., (2019). slurmR: A lightweight wrapper for HPC with Slurm. Journal of Open Source Software,
#' 4(39), 1493, https://doi.org/10.21105/joss.01493
#'
#' A BibTeX entry for LaTeX users is
#'
#' ```
#'  @Article{,
#'   title = {slurmR: A lightweight wrapper for HPC with Slurm},
#'   author = {George {Vega Yon} and Paul Marjoram},
#'   journal = {The Journal of Open Source Software},
#'   year = {2019},
#'   month = {jul},
#'   volume = {4},
#'   number = {39},
#'   doi = {10.21105/joss.01493},
#'   url = {https://doi.org/10.21105/joss.01493},
#' }
#' ```
#' @docType package
#' @name slurmR
NULL

#' @importFrom utils sessionInfo str head
NULL

.onLoad <- function(libname, pkgname) {

  opts_slurmR$set_tmp_path(getwd())
  tmp <- tempfile("slurmR-job-", opts_slurmR$get_tmp_path())

}


.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "On load, `slurmR` sets default options for your jobs: (1) `tmp_path`, which is the ",
    "default directory where `slurmR` will use to create the auxiliar files (default to getwd()), and ",
    "(2) `job-name`, which is the option of the same name in Slurm. You can view/set",
    " these at:\n   ?opts_slurmR\nor you could just type\n   \"opts_slurmR\"."
  )

}
