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
    "slurmR default options for `tmp_path` (used to store auxiliar files) set to:\n  ", getwd(),
    "\nYou can change this and checkout other slurmR options using: ",
    "?opts_slurmR, or you could just type \"opts_slurmR\" on the terminal."
  )

}
