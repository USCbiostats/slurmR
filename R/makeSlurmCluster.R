get_hosts <- function(ntasks=1, tmp_path = getwd(), ...) {

  # In case that the host retrival fails, this should be the exit mechanism
  on.exit({
    if (exists("jobid") && !exists("clean"))
      tryCatch(scancel(jobid), error = function(e) e)
  })

  # Creating job name and file
  fn   <- tempfile("slurmR-job-")
  jn   <- gsub(".+(?=slurmR-job-)", "", fn, perl = TRUE)
  out  <- sprintf("%s/%s.out", tmp_path, jn)

  # Writing the script
  dots <- c(list(ntasks = ntasks, output = out), list(...))
  dat  <- paste(
    "#!/bin/sh",
    paste0("#SBATCH ", parse_flags(dots), collapse = "\n"),
    "echo ==start-hostnames==",
    "srun hostname",
    "sleep infinity", sep = "\n"
  )

  writeLines(dat, fn)

  # Submitting the job
  jobid <- sbatch(fn, wait = FALSE, submit = TRUE)

  # Returning
  hosts <- function() {
    tryCatch({
      hostnames <- suppressWarnings(readLines(out))
      hostnames_start <- which(grepl("^==start-hostnames==$", hostnames)) + 1L
      ans <- hostnames[hostnames_start:(hostnames_start + ntasks - 1L)]
      if (any(is.na(ans)))
         stop("Still reading...", call. = FALSE)
      ans
    }, error = function(e) e)
  }

  clean <- function() suppressWarnings(file.remove(out))

  list(
    hosts  = hosts,
    jobid  = jobid,
    output = out,
    script = fn,
    clean  = clean
  )

}

#' Create a Parallel Socket Cluster using Slurm
#'
#' This function is essentially a wrapper of the function [parallel::makePSOCKcluster].
#' `makeSlurmCluster` main feature is adding node addresses.
#'
#' @template job_name-tmp_path
#' @details By default, if the `time` option is not specified via `...`,
#' then it is set to the value `01:00:00`, this is, 1 hour.
#' @param max_wait Integer scalar. Wait time before exiting with error while
#' trying to read the nodes information.
#' @param cluster_opt A list of arguments passed to [parallel::makePSOCKcluster].
#' @param ... Further arguments passed to [Slurm_EvalQ] via `sbatch_opt`.
#' @param verb Logical scalar. If `TRUE`, the function will print messages on
#' screen reporting on the status of the job submission.
#' @param n Integer scalar. Size of the cluster object (see details).
#'
#' @export
#' @details Once a job is submitted via Slurm, the user gets access to the nodes
#' associated with it, which allows users to star new processes within those.
#' By means of this, we can create Socket, also knwon as "PSOCK", clusters across
#' nodes in a Slurm environment. The name of the hosts are retrieved and passed
#' later on to [parallel::makePSOCKcluster].
#'
#' @section Maximum number of connections:
#'
#' By default, R limits the number of simultaneous connections (see this thread
#' in R-sig-hpc \url{https://stat.ethz.ch/pipermail/r-sig-hpc/2012-May/001373.html})
#' Current maximum is 128 (R version 3.6.1). To modify that limit, you would need
#' to reinstall R updating the macro `NCONNECTIONS` in the file `src/main/connections.c`.
#'
#' For now, if the user sets `n` above 128 it will get an immediate warning
#' pointing to this issue, in particular, specifying that the cluster object
#' may not be able to be created.
#'
#' @return A object of class `c("slurm_cluster", "SOCKcluster", "cluster")`. It
#' is the same as what is returned by [parallel::makePSOCKcluster] with the main
#' difference that it has two extra attributes:
#'
#' - `SLURM_JOBID` Which is the id of the Job that initialized tha cluster.
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
#' # increase the number of workers by increasing the number of child-sessions per job
#'
#' cl <- makeSlurmCluster(9, offspring_per_job = 5) # We leave one job as the master session.
#'
#' # This will have in total 45 child processes!
#' cl
#'
#' }
#'
makeSlurmCluster <- function(
  n,
  job_name       = opts_slurmR$get_job_name(),
  tmp_path       = opts_slurmR$get_tmp_path(),
  cluster_opt    = list(),
  max_wait       = 300L,
  verb           = TRUE,
  ...
  ) {

  if (n > 128L)
    warning(
      "By this version of slurmR, the maximum number of connections in R ",
      "is 128. makeSlurmCluster will try to create the cluster object, ",
      "but it is possible that the function fails to do so (see ?makeSlurmCluster).",
      immediate. = TRUE
      )

  # No need of anything fancy in this case!
  if (opts_slurmR$get_debug())
    return(do.call(parallel::makePSOCKcluster, c(list(names = n), cluster_opt)))

  sbatch_opt <- list(...)

  if (is.null(sbatch_opt$time))
    sbatch_opt$time <- "01:00:00"

  # Creating a job to be submitted
  JOB_PATH <- NULL # THIS IS CREATED JUST TO AVOID THE NOTE DURING R CMD CHECK
  ARRAY_ID <- NULL # THIS IS CREATED JUST TO AVOID THE NOTE DURING R CMD CHECK

  # These are emergency steps taken in case that the user presses break or
  # an error happens after the creating of the job object.
  on.exit({
    if (exists("job") && ( !exists("cl") || !(inherits(cl, "slurm_cluster")))) {
      warning(
        "An error was detected before returning the cluster object. ",
        "If submitted, we will try to cancel the job and stop the cluster ",
        "object.", call. = FALSE, immediate. = TRUE
        )
      e <- tryCatch(parallel::stopCluster(cl), error = function(e) e)
      e <- tryCatch(scancel(job$jobid), error = function(e) e)
      if (inherits(e, "error") && !is.na(job$jobid)) {
        warning(
          "The job ", job$jobid, " is still running. Try canceling it using ",
          "scancel on command line.", call. = FALSE, immediate. = TRUE
          )
      }
    }
  })

  sbatch_opt$ntasks     <- n
  sbatch_opt$tmp_path   <- tmp_path
  sbatch_opt$`job-name` <- job_name

  job <- do.call(get_hosts, sbatch_opt)

  # Now, we wait until the jobs have started. All should return "RUNNING"
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
        message("The job needs to start before continuing. ", appendLF = FALSE)
      } else if (s == 2L) {
        message("The job is running, waiting for the nodenames. ", appendLF = FALSE)
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
    if (!opts_slurmR$get_debug())
      s <- status(job$jobid)

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
    nodenames <- job$hosts()

    if (inherits(nodenames, what = "error"))
      next
    else
      break

  }

  if (inherits(nodenames, what = "error")) {
    scancel(job$jobid)
    stop(
      "While trying to read information regarding the created jobs, one or ",
      "more components failed.",
      call. = FALSE
      )
  }

  if (verb)
    message("Success! nodenames collected (",
    paste(unique(nodenames), collapse=", "),
    "). Creating the cluster object...")

  # Creating the PSOCK cluster
  cl <- do.call(
    parallel::makePSOCKcluster,
    c(list(names = nodenames), cluster_opt)
  )

  attr(cl, "SLURM_JOBID") <- job$jobid
  attr(cl, "class")       <- c("slurm_cluster", attr(cl, "class"))

  # The temp file is no longer needed
  job$clean()

  cl

}

#' @export
#' @param cl An object of class `slurm_cluster`.
#' @rdname makeSlurmCluster
#' @importFrom parallel stopCluster
#' @details The method `stopCluster` for `slurm_cluster` stops the cluster doing
#' the following:
#'
#' 1. Closes the conection by calling the `stopCluster` method for `PSOCK` objects.
#'
#' 2. Cancel the Slurm job using `scancel`.
#'
stopCluster.slurm_cluster <- function(cl) {

  # First, we need to stop the original processes, this will kill the cluster
  # right away!
  # Removing the first class, and calling stop cluster Again!
  class(cl) <- setdiff(class(cl), "slurm_cluster")
  parallel::stopCluster(cl)

  if (!opts_slurmR$get_debug())
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

