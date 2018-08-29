#' @export
print.slurm_job <- function(x, ...) {

  cat("Slurm job: ", deparse(x$call), "\n")
  cat(
    sprintf("job_name: %s", ans$job_name),
    sprintf("job_path: %s\n", ans$job_path), sep="\n"
  )

  invisible(x)
}
