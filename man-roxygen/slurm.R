#' @param job_name Character. Name of the job to be passed to `Slurm`.
#' @param job_path Character. Path to the directory where all the data (including
#' scripts) will be stored (See [opts_sluRm]).
#' @param wait Logical scalar. When `TRUE` waits for the output to return.
#' (see [sbatch]).
#' @param njobs Integer. Number of jobs to specity.
#' @param sbatch_opt,rscript_opt List. Options to be passed via flags to
#' the bash file as `#SBATCH` and to `Rscript` respectively.
#' @param compress Logical scalar (default `TRUE`). Passed to [saveRDS]. Setting
#' this value to `FALSE` can be useful when the user requires faster read/write
#' of R objects on disk.
#' @param seeds Integer vector of length `njobs`. Seeds to be passed to each
#' job.
#' @param export A named list with objects to be included in the Spawned sessions.
#' @param submit Logical, when `TRUE` calls [sbatch] to submit the job to slurm.
#'
NULL
