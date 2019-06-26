#' @param job_name Character. Name of the job to be passed to `Slurm`.
#' @param tmp_path Character. Path to the directory where all the data (including
#' scripts) will be stored. Notice that this path must be accessible by all the
#' nodes in the network (See [opts_sluRm]).
#' @param plan A character scalar. (See [the_plan]).
#' @param njobs Integer. Number of jobs to use in the job-array.
#' @param sbatch_opt,rscript_opt List. Options to be passed via flags to
#' the bash file as `#SBATCH` and to `Rscript` respectively.
#' @param compress Logical scalar (default `TRUE`). Passed to [saveRDS]. Setting
#' this value to `FALSE` can be useful when the user requires faster read/write
#' of R objects on disk.
#' @param seeds Integer vector of length `njobs`. Seeds to be passed to each
#' job. When `NULL` (default), seeds will be picked automatically (see [new_rscript]).
#' @param export A named list with objects to be included in the Spawned sessions.
#' @param libPaths A character vector. See [.libPaths].
#' @param hooks A list of functions (passed to [new_slurm_job]).
#'
NULL
