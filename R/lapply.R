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

  # Setting the job name
  options_sluRm$set_job_path(job_path)
  options_sluRm$set_job_name(job_name)

  # Writing the data on the disk
  INDICES   <- parallel::splitIndices(length(X), nodes)
  dat       <- c(
    list(INDICES = INDICES, X = X, FUN = FUN, mc.cores=mc.cores),
    list(...)
    )
  obj_names <- save_objects(dat)

  # Writing the reading
  rscript   <- ".slurmARRAY_ID <- as.integer(Sys.getenv(\"SLURM_ARRAY_TASK_ID\"))"
  rscript   <- c(
    rscript,
    sprintf(".slurm%s <- readRDS(\"%s/%s/%1$s.rds\")", obj_names,
            options_sluRm$get_job_path(),
            options_sluRm$get_job_name()
            )
  )

  rscript[3] <- paste0(rscript[3], "[.slurmINDICES[[.slurmARRAY_ID]]]")

  # Listing the output
  rscript <- c(
    rscript,
    sprintf(
      "ans <- parallel::mclapply(\n%s\n)",
      paste(sprintf("    %s = .slurm%1$s", obj_names[-1]), collapse=",\n")
    ),
    sprintf("options_sluRm$set_job_path(\"%s\")", options_sluRm$get_job_path()),
    sprintf("options_sluRm$set_job_name(\"%s\")", options_sluRm$get_job_name()),
    "saveRDS(ans, sname(\"rds\", .slurmARRAY_ID))",
    "cat('0', file = sname(\"fin\", .slurmARRAY_ID))"
  )

  rscript <- c(
    rscript_header(),
    structure(
    .Data    = rscript,
    class    = "sluRm_plaintext"
  )
  )

  # Writing the R script out ---------------------------------------------------
  fn_r <- snames("r")
  writeLines(rscript, fn_r)

  # Writing the bash script out ------------------------------------------------
  bashfile <- write_bash(nodes = nodes)

  writeLines(bashfile, snames("sh"))

  # Returning ------------------------------------------------------------------
  structure(
    list(
      call     = match.call(),
      rscript  = rscript,
      bashfile = bashfile,
      robjects = obj_names,
      job_name = options_sluRm$get_job_name(),
      job_path = options_sluRm$get_job_path(),
      nodes    = nodes
    ),
    class    = "slurm_call"
    )

}

