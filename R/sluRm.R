#' A lightweight wrapper for HPC with Slurm
#' @docType package
#' @name sluRm
NULL

#' @importFrom utils sessionInfo str
NULL

.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "On load, `sluRm` sets default options for your jobs (`chdir`, which is the ",
    "default directory where sluRm will use to create the auxiliar files, and ",
    "`job-name`, which is the option of the same name in Slurm. You can view/set",
    " these at:\n   ?opts_sluRm\nor you could just type\n   \"opts_sluRm\"."
  )

}
