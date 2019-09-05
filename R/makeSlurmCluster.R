#' Create a Parallel Socket Cluster using Slurm
#'
#' This function is essentially a wrapper of the function [parallel::makePSOCKcluster].
#' `makeSlurmCluster` main feature is adding node addresses.
#'
#' @template njobs
#' @template job_name-tmp_path
#' @details By default, if the `time` option is not specified via `...`,
#' then it is set to the value `1-0`, this is, 1 day and 0 hours.
#' @param timeout Integer scalar. Wait time before exiting with error while
#' trying to read the nodes information.
#' @param cluster_opt A list of arguments passed to [parallel::makePSOCKcluster].
#' @param ... Further arguments passed to [Slurm_EvalQ] via `sbatch_opt`.
#' @param verb Logical scalar. If `TRUE`, the function will print messages on
#' screen reporting on the status of the job submission.
#' @export
#' @details Once a job is submitted via Slurm, the user gets access to the nodes
#' associated with it, which allows users to star new processes within those.
#' By means of this, we can create PSOCK clusters accross nodes in a Slurm
#' enviornment. In particular, `makeSlurmCluster` performs the following steps:
#'
#' 1. Using [Slurm_EvalQ], a job is submitted using an array with `njobs`.
#'
#' 2. Each job within the array stores information regarding the node where they
#'    are being executed, including the name of the node.
#'
#' 3. Create a PSOCK cluster using the node names obtained from the `Slurm_EvalQ`
#'    call.
#'
#' @examples
#' \dontrun{
#'
#' # Creating a cluster with 200 workers/offpring/child R sessions
#' cl <- makeSlurmCluster(200)
#'
#' # Computing the mean of a 100 random uniforms within each worker
#' # for this we can use any of the function available in the parallel package.
#' ans <- parSapply(1:200, function(x) mean(runif(100)))
#'
#' # We simply call stopCluster as we would do with any other cluster
#' # object
#' stopCluster(ans)
#'
#' # We can also specify SBATCH options directly (...)
#' cl <- makeSlurmCluster(200, partition = "thomas", time = "02:00:00")
#' stopCluster(cl)
#' }
#'
makeSlurmCluster <- function(
  njobs,
  job_name    = opts_sluRm$get_job_name(),
  tmp_path    = opts_sluRm$get_tmp_path(),
  cluster_opt = list(),
  timeout     = 300L,
  verb        = TRUE,
  ...
  ) {

  sbatch_opt <- list(...)

  if (is.null(sbatch_opt$time))
    sbatch_opt$time <- "1-0"

  # Creating a job to be submitted
  JOB_PATH <- NULL # THIS IS CREATED JUST TO AVOID THE NOTE DURING R CMD CHECK
  ARRAY_ID <- NULL # THIS IS CREATED JUST TO AVOID THE NOTE DURING R CMD CHECK
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


  # Let's just wait a few seconds before jumping into conclusions!
  Sys.sleep(1)

  ntry <- -1L
  while ((Sys.time() - time0) <= timeout) {

    # For sanity, will wait for half a second
    Sys.sleep(.5)
    ntry <- ntry + 1L

    if (verb && ntry > 0L && !(ntry %% 5)) {
       message(
         "Some jobs need to be allocated still (", njobs - length(attr(s, "pending")),
         " out of ", njobs,
         "). Waiting for a few more seconds before trying again..."
       )
       Sys.sleep(3)
    }

    # In the case of debug mode on, we don't need to check for the job status.
    # we can go directly to the R sessions
    if (!opts_sluRm$get_debug())
      s <- status(job)

    # One or more failed
    if (s == 99L) {
      scancel(job$jobid)
      stop(attr(s, "description"), call. = FALSE)
    } else if (s == 0L) {
      scancel(job$jobid)
      stop("The job was completed. This shouldn't happen!", call. = FALSE)
    } else if (s == -1L)
      next

    # Trying to read the dataset
    info <- lapply(files2read, try_readRDS)

    if (any(sapply(info, inherits, what = "error")))
      next
    else
      break

  }

  if (any(sapply(info, inherits, what = "error"))) {
    scancel(job$jobid)
    stop(
      "While trying to read information regarding the created jobs, one or ",
      "more components failed.",
      call. = FALSE
      )
  }

  if (verb)
    message("Success! All jobs have been allocated. Creating the cluster object...")

  # Extracting the relevant information
  pids      <- sapply(info, "[[", "pid")
  nodenames <- sapply(info, function(i) i$who["SLURMD_NODENAME"])

  # Creating the PSOCK cluster
  cl <- do.call(
    parallel::makePSOCKcluster,
    c(list(names = nodenames), cluster_opt)
  )

  attr(cl, "SLURM_PIDS")  <- pids
  attr(cl, "SLURM_JOBID") <- job$jobid
  attr(cl, "class")       <- c("SlurmCluster", attr(cl, "class"))

  cl

}

#' @export
#' @param cl An object of class `SlurmCluster`.
#' @rdname makeSlurmCluster
#' @importFrom parallel stopCluster
#' @details The method `stopCluster` for `SlurmCluster` stops the cluster doing
#' the following:
#'
#' - Then, calls the `stopCluster` method for `PSOCK` objects.
#'
#' - Cancel the Slurm job using `scancel`.
#'
stopCluster.SlurmCluster <- function(cl) {

  # First, we need to stop the original processes, this will kill the cluster
  # right away!
  # Removing the first class, and calling stop cluster Again!
  class(cl) <- setdiff(class(cl), "SlurmCluster")
  parallel::stopCluster(cl)

  if (!opts_sluRm$get_debug())
    scancel(attr(cl, "SLURM_JOBID"))

  invisible()

}
