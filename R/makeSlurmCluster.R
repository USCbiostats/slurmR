#' Create a Parallel Socket Cluster using Slurm
#'
#'
#' @template job_name-tmp_path
#' @details By default, if the `time` option is not specified via `sbatch_opt`,
#' then it is set to the value `1-0`, this is, 1 day and 0 hours.
#' @export
#'
makeSlurmCluster <- function(
  njobs,
  job_name   = opts_sluRm$get_job_name(),
  tmp_path   = opts_sluRm$get_tmp_path(),
  sbatch_opt = list(),
  timeout = 300L,
  ...
  ) {

  if (is.null(sbatch_opt$time))
    sbatch_opt$time <- "1-0"

  # Creating a job to be submitted
  job <- Slurm_EvalQ(expr = {

    # Saving the process id... so we can kill it!
    jobinfo <- list(pid = Sys.getpid(), who = sluRm::whoami())
    saveRDS(jobinfo, normalizePath(sprintf("%s/jobinfo%05d.rds", JOB_PATH, ARRAY_ID)))

    # Wait ad-infinitum -- The job will be killed eitherway once the
    while (TRUE) {
      Sys.sleep(100)
    }

    },
    njobs       = njobs,
    job_name    = job_name,
    tmp_path    = tmp_path,
    sbatch_opt  = sbatch_opt,
    plan        = "submit",
    rscript_opt = list(vanilla = TRUE, slave = TRUE)
    )


  # Now, we wait until the jobs have started. All should return "RUNNING"
  time0       <- Sys.time()
  s           <- 1L
  try_readRDS <- function(...) tryCatch({
    suppressWarnings(readRDS(...))
    }, error = function(e) e)

  files2read  <- ifelse(opts_sluRm$get_debug(), 1L, njobs)
  files2read  <- suppressWarnings(normalizePath(
    sprintf("%s/%s/jobinfo%05d.rds", tmp_path, job_name , 1L:files2read)
    ))

  while ((Sys.time() - time0) <= timeout) {

    # In the case of debug mode on, we don't need to check for the job status.
    # we can go directly to the R sessions
    if (!opts_sluRm$get_debug())
      s <- state(job)

    # One or more failed
    if (s == 2L)
      stop("One or more jobs failed to initialize.", call. = FALSE)
    if (s == 0L)
      stop("The job was completed. This shouldn't happen!", call. = FALSE)
    if (s == -1L)
      next

    # Trying to read the dataset
    info <- lapply(files2read, try_readRDS)

    if (any(sapply(info, inherits, what = "error")))
      next
    else
      break

  }

  if (any(sapply(info, inherits, what = "error")))
    stop(
      "While trying to read information regarding the created jobs, one or ",
      "more components failed.",
      call. = FALSE
      )

  # Extracting the relevant information
  pids      <- sapply(info, "[[", "pid")
  nodenames <- sapply(info, function(i) i$who["SLURM_NODENAME"])


  # Creating the PSOCK cluster
  cl <- parallel::makePSOCKcluster(
    nodenames, ...
  )

  attr(cl, "SLURM_PIDS") <- pids
  attr(cl, "class")      <- c("SlurmCluster", attr(cl, "class"))

  cl

}

#' @export
#' @rdname makeSlurmCluster
#' @importFrom parallel stopCluster parSapply
stopCluster.SlurmCluster <- function(cl) {

  # First, we need to stop the original processes, this will kill the cluster
  # right away!
  ans <- parallel::parSapply(
    cl  = cl,
    X   = attr(cl, "SLURM_PIDS"),
    FUN = function(x.) silent_system2("kill", x., stdout = TRUE, stderr = TRUE),
    .scheduling = "static"
    )

  if (any(sapply(ans, inherits, what = "error")))
    warning("One or more jobs were not killed.", call. = FALSE)

  # Removing the first class, and calling stop cluster Again!
  class(cl) <- setdiff(class(cl), "SlurmCluster")
  parallel::stopCluster(cl)

}
