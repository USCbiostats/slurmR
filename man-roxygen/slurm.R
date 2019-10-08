#' @param plan A character scalar. (See [the_plan]).
#' @param compress Logical scalar (default `TRUE`). Passed to [saveRDS]. Setting
#' this value to `FALSE` can be useful when the user requires faster read/write
#' of R objects on disk.
#' @param seeds Integer vector of length `njobs`. Seeds to be passed to each
#' job. When `NULL` (default), seeds will be picked automatically (see [new_rscript]).
#' @param export A named list with objects to be included in the Spawned sessions.
#' @param export_env An environment. Environment where the objects listed in
#' `export` are located (default [parent.frame()]). 
#' @param libPaths A character vector. See [.libPaths].
#' @param hooks A list of functions (passed to [new_slurm_job]).
#'
NULL
