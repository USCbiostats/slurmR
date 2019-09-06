#' Create a Parallel Socket Cluster using Slurm
#'
#' This function is essentially a wrapper of the function [parallel::makePSOCKcluster].
#' `makeSlurmCluster` main feature is adding node addresses.
#'
#' @template njobs
#' @template job_name-tmp_path
#' @details By default, if the `time` option is not specified via `...`,
#' then it is set to the value `01:00:00`, this is, 1 hour.
#' @param max_wait Integer scalar. Wait time before exiting with error while
#' trying to read the nodes information.
#' @param cluster_opt A list of arguments passed to [parallel::makePSOCKcluster].
#' @param ... Further arguments passed to [Slurm_EvalQ] via `sbatch_opt`.
#' @param verb Logical scalar. If `TRUE`, the function will print messages on
#' screen reporting on the status of the job submission.
#' @param ntasks Integer scalar. Number of R sessions to start per job (see details).
#'
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
#' The number of workers/childs/sessions that will be used is the product between
#' `ntasks` and `njobs`. In some Slurm settings users may be constrained in the
#' number of jobs that can be executed simultaneously, hence, increasing the
#' number of tasks per job may be a solution to increase the number of workers.
#'
#' @section Maximum number of connections:
#'
#' By default, R limits the number of simultaneous connections (see this thread
#' in R-sig-hpc \url{https://stat.ethz.ch/pipermail/r-sig-hpc/2012-May/001373.html})
#' Current maximum is 128 (R version 3.6.1). To modify that limit, you would need
#' to reinstall R updating the macro `NCONNECTIONS` in the file `src/main/connections.c`.
#'
#' For now, if the user sets `njobs` above 128 it will get an inmediate warning
#' pointing to this issue, in particular, specifying that the cluster object
#' may not be able to be created.
#'
#' @return A object of class `c("slurm_cluster", "SOCKcluster", "cluster")`. It
#' is the same as what is returned by [parallel::makePSOCKcluster] with the main
#' difference that it has two extra attributes:
#'
#' - `SLURM_JOBID` Which is the id of the Job that initialized tha cluster.
#'
#' - `SLURM_PIDS` Which is an integer vector of the PIDs of the R processes that
#'   the job started in the remote machines.
#'
#' @examples
#' \dontrun{
#'
#' # Creating a cluster with 100 workers/offpring/child R sessions
#' cl <- makeSlurmCluster(100)
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
#'
#' # Limited number of simultaneous jobs ---------------------------------------
#' # Suppose that the user cannot run more than 10 jobs at a time. We can
#' # increase the number of workers by increasing the number of tasks
#'
#' cl <- makeSlurmCluster(9, ntasks = 5) # We leave one job as the master session.
#'
#' # This will have in total 45 child processes!
#' cl
#'
#' }
#'
makeSlurmCluster <- function(
  njobs,
  job_name    = opts_sluRm$get_job_name(),
  tmp_path    = opts_sluRm$get_tmp_path(),
  cluster_opt = list(),
  max_wait    = 300L,
  verb        = TRUE,
  ntasks      = 1L,
  ...
  ) {

  if (njobs * ntasks > 128L)
    warning(
      "By this version of sluRm, the maximum number of connections in R ",
      "is 128. makeSlurmCluster will try to create the cluster object, ",
      "but it is possible that the function fails to do so (see ?makeSlurmCluster).",
      immediate. = TRUE
      )

  sbatch_opt        <- list(...)
  sbatch_opt$ntasks <- ntasks

  if (is.null(sbatch_opt$time))
    sbatch_opt$time <- "01:00:00"

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
  try_readRDS <- function(...) tryCatch({
    suppressWarnings(readRDS(...))
    }, error = function(e) e)

  files2read  <- ifelse(opts_sluRm$get_debug(), 1L, njobs)
  files2read  <- suppressWarnings(normalizePath(
    sprintf("%s/%s/jobinfo%05d.rds", tmp_path, job_name , 1L:files2read)
    ))


  # Let's just wait a few seconds before jumping into conclusions!
  Sys.sleep(1)
  ntry  <- -1L
  time0 <- Sys.time()
  s     <- 1L
  while (difftime(Sys.time(), time0, units = "secs") <= max_wait) {

    # For sanity, will wait for half a second
    Sys.sleep(.5)
    ntry <- ntry + 1L

    if (verb && ntry > 0L && !(ntry %% 5)) {

      if (s %in% c(1L, 3L)) {
        message(
          sprintf(
             "%4d/%-4d jobs need to start before continuing.",
             length(attr(s, "pending")), njobs
             ), appendLF = FALSE
        )
      } else if (s == 2L) {
        message("All jobs are running, waiting for the nodenames ", appendLF = FALSE)
      }

      # Common message
      message(
        sprintf(
          "Remaining wait time: %-4d(s).",
          max_wait - as.integer(difftime(Sys.time(), time0, units = "s"))
        )
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
    } else if (s == 1L)
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
    c(list(names = rep(nodenames, ntasks)), cluster_opt)
  )

  attr(cl, "SLURM_PIDS")  <- pids
  attr(cl, "SLURM_JOBID") <- job$jobid
  attr(cl, "class")       <- c("slurm_cluster", attr(cl, "class"))

  cl

}

#' @export
#' @param cl An object of class `slurm_cluster`.
#' @rdname makeSlurmCluster
#' @importFrom parallel stopCluster
#' @details The method `stopCluster` for `slurm_cluster` stops the cluster doing
#' the following:
#'
#' - Then, calls the `stopCluster` method for `PSOCK` objects.
#'
#' - Cancel the Slurm job using `scancel`.
#'
stopCluster.slurm_cluster <- function(cl) {

  # First, we need to stop the original processes, this will kill the cluster
  # right away!
  # Removing the first class, and calling stop cluster Again!
  class(cl) <- setdiff(class(cl), "slurm_cluster")
  parallel::stopCluster(cl)

  if (!opts_sluRm$get_debug())
    scancel(attr(cl, "SLURM_JOBID"))

  invisible()

}

#' @export
print.slurm_cluster <- function(x, ...) {

  cat("A Slurm SOCK cluster (jobid: ", attr(x, "SLURM_JOBID"), ")\n", sep = "")
  class(x) <- setdiff(class(x), "slurm_cluster")
  print(x)
  invisible(NULL)

}
