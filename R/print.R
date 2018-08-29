#' @export
print.slurm_job <- function(x, ...) {

  cat("Call:\n", paste(deparse(x$call), collapse="\n"), "\n")
  cat(
    sprintf("job_name : %s\n", x$job_name),
    sprintf("job_path : %s\n", x$job_path),
    sprintf("job ID   : %s\n",
      ifelse(
        is.na(x$job_id),
        "Not submitted",
        as.character(x$job_id)
        )
      ), sep=""
  )

  invisible(x)
}
