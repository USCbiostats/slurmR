#' Collect the results
#' @param ... Further arguments passed to the method.
#' @param x An object of class `slurm_call`.
#' @export
Slurm_collect <- function(...) UseMethod("Slurm_collect")

#' @export
#' @param any Logical. When `TRUE`, it will collect any output available regardless
#' of whether the job is completed or not.
#' @rdname Slurm_collect
Slurm_collect.slurm_job <- function(x, any = TRUE, ...) {

  # Making sure the previous setup is kept -------------------------------------
  old_job_name <- opts_sluRm$get_job_name(check = FALSE)
  old_chdir    <- opts_sluRm$get_chdir()

  on.exit({
    opts_sluRm$set_job_name(old_job_name, check = FALSE, overwrite = FALSE)
    opts_sluRm$set_chdir(old_chdir)
  })

  # Setting the job_status -----------------------------------------------------
  opts_sluRm$set_chdir(x$job_opts$chdir)
  opts_sluRm$set_job_name(x$job_opts$`job-name`, overwrite = FALSE)

  if (!opts_sluRm$get_debug()) {
    S <- state(x)

    # Getting the filenames
    if (!S)
      do.call("c", lapply(snames("rds", 1:x$njobs), readRDS))
    else if (any && (S == 1))
      do.call("c", lapply(snames("rds", attr(S, "done")), readRDS))
    else
      stop("Nothing to retrieve. (see ?status).", call. = FALSE)

  } else {

    fn <- snames("rds", 1L)
    if (file.exists(fn))
      readRDS(fn)
    else
      stop("No result yet from the script.", call. = FALSE)

  }

}
