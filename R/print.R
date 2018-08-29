#' @export
print.slurm_job <- function(x, ...) {

  cat("Slurm job: ", deparse(x$call), "\n")
  cat(
    sprintf("job_name: %s", x$job_name),
    sprintf("job_path: %s\n", x$job_path), sep="\n"
  )

  invisible(x)
}
