#' A Lightweight Wrapper for 'Slurm'
#' @details
#' To cite slurrm in publications use:
#'
#' Vega Yon et al., (2019). sluRm: A lightweight wrapper for HPC with Slurm. Journal of Open Source Software,
#' 4(39), 1493, https://doi.org/10.21105/joss.01493
#'
#' A BibTeX entry for LaTeX users is
#'
#' ```
#'  @Article{,
#'   title = {sluRm: A lightweight wrapper for HPC with Slurm},
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
#' @name slurrm
NULL

#' @importFrom utils sessionInfo str head
NULL

.onLoad <- function(libname, pkgname) {

  opts_sluRm$set_tmp_path(getwd())

  tmp <- tempfile("sluRm-job-", opts_sluRm$get_tmp_path())
  tmp <- gsub(".+(?=sluRm-job-)", "", tmp, perl = TRUE)

  opts_sluRm$set_job_name(tmp)
}


.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "On load, `slurrm` sets default options for your jobs: (1) `tmp_path`, which is the ",
    "default directory where `slurrm` will use to create the auxiliar files (default to getwd()), and ",
    "(2) `job-name`, which is the option of the same name in Slurm. You can view/set",
    " these at:\n   ?opts_sluRm\nor you could just type\n   \"opts_sluRm\"."
  )

}
