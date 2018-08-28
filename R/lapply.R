#' The Slurm version of the `lapply`  function
#' @param X,FUN,mc.cores,... Arguments passed to [parallel::mclapply].
#' @param write_bash_args List of arguments passed to [write_bash_args].
#' @template slurm
#' @export
#' @examples
#' Slurm_lapply(1:20, mean)
Slurm_lapply <- function(
  X,
  FUN,
  ...,
  write_bash_args = list(),
  nodes    = 2,
  mc.cores = getOption("mc.cores", 2L),
  job_name = getOption("sluRm.job_name", "sluRm"),
  job_path = NULL
  ) {

  # Checking the path
  if (!length(job_path))
    job_path <- options_sluRm$get_job_path()

  # Writing the data on the disk
  INDICES   <- parallel::splitIndices(length(X), nodes)
  dat       <- c(list(INDICES = INDICES, X = X, FUN = FUN), list(...))
  obj_names <- save_objects(dat, job_path = job_path, job_name = job_name)

  # Writing the reading
  rscript   <- ".slurmARRAY_ID <- as.integer(Sys.getenv(\"SLURM_ARRAY_TASK_ID\"))"
  rscript   <- c(
    rscript,
    sprintf(".slurm%s <- readRDS(\"%s/%s/%1$s.rds\")", obj_names, job_path, job_name)
  )

  rscript[3] <- paste0(rscript[3], "[.slurmINDICES[[.slurmARRAY_ID]]]")

  # Listing the output
  rscript <- c(
    rscript,
    sprintf(
      "ans <- parallel::mclapply(\n%s\n)",
      paste(sprintf("    %s = .slurm%1$s", obj_names[-1]), collapse=",\n")
    ),
    sprintf(
      "saveRDS(ans, sprintf(\"%s/%s/ans%%02i.rds\", .slurmARRAY_ID))",
      job_path, job_name
    )
  )

  rscript <- c(
    rscript_header(),
    structure(
    .Data    = rscript,
    class    = "sluRm_plaintext"
  )
  )

  # Creating file name and writing
  fn_r <- sprintf("%s/%s/rscript.r", job_path, job_name)

  writeLines(
    rscript,
    fn_r
    )

  bashfile <- write_bash(fn_r, job_name = job_name, job_path = job_path,
                         nodes = nodes)

  fn_sh <- sprintf("%s/%s/bash.sh", job_path, job_name)
  writeLines(
    bashfile,
    fn_sh
  )

  rscript <- structure(
    rscript,
    robjects = obj_names,
    job_name = job_name,
    job_path = job_path
    )


  invisible(rscript)

}
