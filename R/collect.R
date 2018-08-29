#' Collect the results
#' @export
Slurm_collect <- function(...) UseMethod("Slurm_collect")

#' @export
#' @rdname Slurm_collect
Slurm_collect.slurm_call <- function(x, ...) {

  # Making sure the previous setup is kept -------------------------------------
  old_job_name <- options_sluRm$get_job_name(check = FALSE)
  old_job_path <- options_sluRm$get_job_path()

  on.exit({
    options_sluRm$set_job_name(old_job_name, check = FALSE, overwrite = FALSE)
    options_sluRm$set_job_path(old_job_path)
  })

  # Setting the job_status -----------------------------------------------------
  options_sluRm$set_job_name(x$job_name, overwrite = FALSE)
  options_sluRm$set_job_path(x$job_path)

  # Getting the filenames
  fn <- snames("fin", 1:x$nodes)

  test <- file.exists(fn)
  cat(sprintf("Status of job '%s':\n", x$job_name))
  cat(sprintf(
    " - Task %03i/%03i: %s\n", 1:x$nodes, x$nodes,
    ifelse(test, "Complete.", "Pending.")
    ), sep=""
    )

  if (any(test))
    structure(
      lapply(fn[test], readRDS),
      names = which(test))
  else
    NULL

}
