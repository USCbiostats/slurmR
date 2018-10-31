#' Collect the results
#' @param ... Further arguments passed to the method.
#' @param x An object of class `slurm_call`.
#' @export
Slurm_collect <- function(...) UseMethod("Slurm_collect")

#' @export
#' @rdname Slurm_collect
Slurm_collect.slurm_job <- function(x, ...) {

  # Making sure the previous setup is kept -------------------------------------
  old_job_name <- opts_sluRm$get_job_name(check = FALSE)
  old_chdir    <- opts_sluRm$get_chdir()

  on.exit({
    opts_sluRm$set_job_name(old_job_name, check = FALSE, overwrite = FALSE)
    opts_sluRm$set_chdir(old_chdir)
  })

  # Setting the job_status -----------------------------------------------------
  opts_sluRm$set_job_name(x$job_opts$`job-name`, overwrite = FALSE)
  opts_sluRm$set_chdir(x$job_opts$chdir)

  # Getting the filenames
  fn <- snames("fin", 1:x$njobs)

  test <- file.exists(fn)
  cat(sprintf("Status of job '%s':\n", x$job_opts$`job-name`))
  cat(sprintf(
    " - Task %03i/%03i: %s\n", 1:x$njobs, x$njobs,
    ifelse(test, "Complete.", "Pending.")
    ), sep=""
    )

  if (any(test))
    do.call("c", lapply(snames("rds", which(test)), readRDS))
  else
    NULL

}
