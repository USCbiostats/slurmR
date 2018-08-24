#' The Slurm version of the `lapply`  function
#' @param X,FUN,mc.cores,... See [parallel::mclapply].
#' @template slurm
#' @export
Slurm_lapply <- function(
  X,
  FUN,
  ...,
  mc.cores = getOption("mc.cores", 2L),
  job_name = getOption("sluRm.job_name", "sluRm"),
  job_path = tempdir()
  ) {

  # Checking the path

  # Writing the data on the disk
  dat       <- c(list(X = X, FUN = FUN), list(...))
  obj_names <- save_objects(dat, job_path, job_name)

  # Writing the reading
  rscript   <- sprintf(".slurm%s <- readRDS(\"%s/%s/%1$s.rds\")", obj_names, job_path, job_name)

  # Listing the output
  rscript <- c(
    rscript,
    sprintf(
      "parallel::mclapply(\n%s\n)",
      paste(sprintf("    %s = .slurm%1$s", obj_names), collapse=",\n")
    )
  )

  structure(
    .Data    = rscript,
    class    = "sluRm_plaintext",
    robjects = obj_names,
    job_name = job_name,
    job_path = job_path
    )

}
