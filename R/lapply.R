#' The Slurm version of the `lapply`  function
#' @param X,FUN,mc.cores,... Arguments passed to [parallel::mclapply].
#' @param submit Logical, when `TRUE` calls [sbatch] to submit the job to slurm.
#' @template slurm
#' @param wait Logical scalar. When `TRUE` waits for the output to return.
#' (see [sbatch]).
#' @param nodes Integer. Number of nodes to specity.
#' @param sbatch_opt,rscript_opt List. Options to be passed via flags to
#' the bash file as `#SBATCH` and to `Rscript` respectively.
#' @param compress Logical scalar (default `TRUE`). Passed to [saveRDS]. Setting
#' this value to `FALSE` can be useful when the user requires faster read/write
#' of R objects on disk.
#' @export
#' @examples
#' \dontrun{
#'   # A job drawing 1e6 uniforms on 20 nodes.
#'   # The option wait=TRUE makes it return only once the job is completed.
#'   job1 <- Slurm_lapply(1:20, function(i) runif(1e6), nodes=20, wait = TRUE)
#'
#'   # We can collect
#'   ans <- Slurm_collect(ans1)
#'
#'   # Same as before, but not waiting this time, and we are passing more
#'   # arguments to the function
#'   job1 <- Slurm_lapply(1:20, function(i, a) runif(1e6, a), a = -1, nodes=20,
#'       wait = FALSE)
#'
#'   # We can submit
#'   job1 <- sbatch(job1)
#'
#'   # And cancel a job
#'   scancel(job1)
#' }
Slurm_lapply <- function(
  X,
  FUN,
  ...,
  nodes    = 2,
  mc.cores = getOption("mc.cores", 2L),
  job_name = getOption("sluRm.job_name", "sluRm"),
  job_path = NULL,
  submit   = TRUE,
  wait     = TRUE,
  sbatch_opt  = list(),
  rscript_opt = list(vanilla=TRUE),
  compressed = TRUE
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
  obj_names <- save_objects(dat, compress = compress)

  # R Script -------------------------------------------------------------------

  # Initializing the script
  rscript <- new_rscript()

  # Adding readRDS
  rscript$add_rds(obj_names[-2])
  rscript$add_rds(obj_names[2], TRUE)

  # Adding actual code
  rscript$append(
    sprintf(
      "ans <- parallel::mclapply(\n%s\n)",
      paste(sprintf("    %s = .slurm%1$s", obj_names[-1]), collapse=",\n")
    )
  )

  # Finalizing and writing it out
  rscript$finalize(compress = compress)
  rscript$write()


  # Writing the bash script out ------------------------------------------------
  bash <- new_bash(nodes = nodes)
  bash$add_SBATCH(sbatch_opt)
  bash$finalize(rscript_opt)
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- structure(
    list(
      call     = match.call(),
      rscript  = snames("r"),
      bashfile = snames("sh"),
      robjects = obj_names,
      job_name = options_sluRm$get_job_name(),
      job_path = options_sluRm$get_job_path(),
      nodes    = nodes,
      job_id   = NA
    ),
    class    = "slurm_job"
    )

  if (submit)
    return(sbatch(ans, wait = wait))

  ans

}

