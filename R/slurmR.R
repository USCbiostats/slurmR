#' A Lightweight Wrapper for 'Slurm'
#'
#' 'Slurm', Simple Linux Utility for Resource Management
#' \url{https://slurm.schedmd.com/}, is a popular 'Linux' based software used to
#' schedule jobs in 'HPC' (High Performance Computing) clusters. This R package
#' provides a specialized lightweight wrapper of 'Slurm' with a syntax similar to
#' that found in the 'parallel' R package. The package also includes a method for
#' creating socket cluster objects spanning multiple nodes that can be used with
#' the 'parallel' package.
#'
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

  # It will set either the envir var SLURMR_TMP_PATH or getwd()
  opts_slurmR$set_tmp_path()

}


.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "slurmR default option for `tmp_path` (used to store auxiliar files) set to:\n  ", opts_slurmR$get_tmp_path(),
    "\nYou can change this and checkout other slurmR options using: ",
    "?opts_slurmR, or you could just type \"opts_slurmR\" on the terminal."
  )

}

