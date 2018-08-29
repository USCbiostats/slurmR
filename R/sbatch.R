#' A wrapper of `sbatch`.
#'
#' @param x An object of class `slurm_job`.
#' @export
sbatch <- function(x) UseMethod("sbatch")

#' @export
#' @rdname sbatch
sbatch.slurm_job <- function(x) {

  if (!is.na(x$job_id))
    stop("Job ", x$job_id," is already running.")

  ans <- tryCatch(system(paste0("sbatch ", x$job_id), intern=TRUE), error=function(e) e)
  if (inherits(ans, "error"))
    stop("An error has occurred when calling `sbatch`: ", ans$message)

  x$job_id <- as.integer(gsub(pattern = ".+ (?=[0-9]+$)", "", ans, perl=TRUE))

  x

}

#' @export
#' @rdname sbatch
scancel <- function(x) UseMethod("scancel")

#' @export
#' @rdname sbatch
scancel.slurm_job <- function(x) {

  if (is.na(x$job_id)) {
    warning("This job hasn't started yet. Nothing to cancel.", call. = FALSE)
    return()
  }

  ans <- tryCatch(system(paste0("scancel ", x$job_id), intern=TRUE), error=function(e) e)
  if (inherits(ans, "error"))
    stop("An error has occurred when calling `scancel`: ", ans$message)

  invisible()

}
