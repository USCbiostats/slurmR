#' Check the status of a Slurm JOB
#'
#' Using the [sacct] function, it checks the status of a particular job and
#' returns information about its current state, with details regarding the
#' jobs (if an array) that are done, running, pending, or failed.
#'
#' @param x Either a Job id, an object of class `slurm_job`, or an object of
#' class `slurm_status`.
#' @param clusters Character scalar. Name of the cluster where the job is (flag
#' `--clusters` of `sacct`). Only needed when `x` is a job id and the job is
#' not in the local cluster; `slurm_job` objects remember the cluster where they
#' were submitted.
#' @param ... Ignored.
#'
#' @return An integer with attributes of class `slurm_status`. The attributes
#' are integer vectors indicating which jobs fail in the categories of `done`,
#' `failed`, `pending`, and `running` (see [JOB_STATE_CODES]). Possible return
#' values are:
#'
#'
#' - `-1`: No job found. This may be a false negative as the job may still be
#'   on it's way to be submitted.
#'
#' - `0`: Job completed.
#'
#' - `1`: All jobs are pending resource allocation or are on it's way to start.
#'
#' - `2`: All jobs are currently running.
#'
#' - `3`: One or more jobs are still running.
#'
#' - `99`: One or more jobs failed.
#'
#' If the job is not an array, then function will return the corresponding code
#' but the attributes will only have a single number, 1, according to the state
#' of the job (completed, failed, pending).
#'
#' @export
#' @family utilities
#' @family post submission
#' @examples
#' \dontrun{
#'
#' x <- Slurm_EvalQ(Sys.sleep(100), njobs = 2)
#'
#' status(x) # A possible result: An integer with attributes
#' # Status: All jobs are pending resource allocation or are on it's way to start. (Code 1)
#' # This is a job array. The status of each job, by array id, is the following:
#' # done      :  -
#' # failed    :  -
#' # pending   : 1, 2.
#' # running   :  -
#'
#' }
status <- function(x, ...) UseMethod("status")

#' @export
#' @rdname status
status.slurm_job <- function(x, ...) {
  # Passing the job object along so that, if -sacct- fails (e.g., no
  # accounting), the fallback can still find the job's files (see #29 and #40).
  status_impl(get_job_id(x), job = x)
}

#' @export
#' @rdname status
status.default <- function(x, clusters = NULL, ...) {
  status_impl(x, clusters = clusters)
}

#' @param x A job ID.
#' @param job Either `NULL` or an object of class `slurm_job`. When not `NULL`,
#' it is passed to [sacct_] instead of `x`.
#' @param clusters Either `NULL` or the name of the cluster where the job is
#' (only used when `job` is `NULL`; otherwise the job's cluster is used).
#' @noRd
status_impl <- function(x, job = NULL, clusters = NULL) {

  wrap <- function(val, S) {

    desc <- switch(
      as.character(val),
      `-1` = "No job found. This may be a false negative as the job may still be on it's way to be submitted.",
      `0`  = "Job completed.",
      `1`  = "All jobs are pending resource allocation or are on it's way to start.",
      `2`  = "All jobs are currently running.",
      `3`  = "One or more jobs are running.",
      `99` = "One or more jobs failed."
    )

    do.call(
      structure,
      c(list(
        .Data       = val,
        description = desc,
        class       = "slurm_status",
        njobs       = if (exists("njobs")) njobs else 0L
      ), S))
  }

  # Checking the data
  if (is.na(x))
    return(wrap(-1L, NULL))

  dat <- sacct_(
    if (is.null(job)) x else job,
    brief       = TRUE,
    parsable    = TRUE,
    allocations = TRUE,
    clusters    = clusters
  )

  # We only need to keep the main line of the account
  if (!nrow(dat))
    return(wrap(-1L, NULL))

  # Processing ids
  dat. <- NULL
  for (i in 1L:nrow(dat)) {

    # Expanding Array indexes, and the data retrieved from sacct.
    idx <- expand_array_indexes(dat$JobID[i])

    if (i == 1L) {
      dat. <- suppressWarnings(cbind(dat[i, , drop=FALSE], NewId = idx))
    } else {

      dat. <- suppressWarnings(rbind(
        dat.,
        cbind(dat[i, , drop=FALSE], NewId = idx),
        stringsAsFactors = FALSE,
        make.row.names   = FALSE
      ))

    }

  }

  dat <- subset(dat., select = c(-JobID))
  rownames(dat) <- seq_len(nrow(dat))
  colnames(dat)[ncol(dat)] <- "JobID"

  # Filtering the data, we don't use the steps, just the jobs.
  JobID <- dat$JobID
  State <- dat$State

  # A slurm_job remembers the array selection used by its most recent sbatch
  # call. Some Slurm installations omit cancelled array elements from sacct,
  # so rows returned by sacct alone do not define the expected array size.
  expected <- NULL
  if (!is.null(job)) {
    expected <- job$array_ids
    if (!length(expected))
      expected <- seq_len(job$njobs) # Jobs saved by slurmR <= 0.6-0
    expected <- as.integer(expected)
  }

  STATE_CODES <- split(JOB_STATE_CODES$name, JOB_STATE_CODES$type)


  # Convert Slurm job IDs to array task IDs. A single unadorned job ID belongs
  # to the sole expected task (normally task 1).
  has_array_id <- grepl("_", JobID, fixed = TRUE)
  JobID[has_array_id] <- sub(".+_", "", JobID[has_array_id])
  if (length(expected) == 1L)
    JobID[!has_array_id] <- expected
  JobID <- suppressWarnings(as.integer(JobID))

  if (length(expected)) {

    # Ignore allocation rows that cannot be mapped to an array task, then ask
    # squeue about expected IDs omitted by sacct. If an ID is in neither data
    # source, it is no longer queued and did not complete, so it failed.
    keep  <- !is.na(JobID) & JobID %in% expected
    JobID <- JobID[keep]
    State <- State[keep]

    missing <- setdiff(expected, JobID)
    if (length(missing)) {
      queued <- tryCatch(squeue(job), error = function(e) e)
      queued_ids <- integer(0)
      queued_states <- character(0)

      if (!inherits(queued, "error") && nrow(queued)) {
        state_col <- intersect(c("STATE", "STATUS", "ST"), names(queued))
        if (length(state_col) && "JOBID" %in% names(queued)) {
          for (i in seq_len(nrow(queued))) {
            ids <- expand_array_indexes(queued$JOBID[i])
            ids <- suppressWarnings(as.integer(sub(".+_", "", ids)))
            use <- !is.na(ids) & ids %in% missing
            queued_ids <- c(queued_ids, ids[use])
            queued_states <- c(
              queued_states,
              rep(queued[[state_col[1L]]][i], sum(use))
            )
          }
        }
      }

      JobID <- c(JobID, queued_ids, setdiff(missing, queued_ids))
      State <- c(
        State,
        queued_states,
        rep("FAILED", length(setdiff(missing, queued_ids)))
      )
    }

    ord <- match(expected, JobID)
    JobID <- JobID[ord]
    State <- State[ord]
  }

  njobs <- if (length(expected)) length(expected) else length(State)
  State <- lapply(STATE_CODES, function(jsc) {
    m <- grepl(paste0(jsc, collapse = "|"), State)
    JobID[which(m)]
  })

  if (length(State$done) == njobs) {

    return(wrap(0L, State))

  } else if (length(State$pending) == njobs) {

    return(wrap(1L, State))

  } else if (length(State$running) == njobs) {

    return(wrap(2L, State))

  } else if (length(State$failed) > 0L) {

    return(wrap(99L, State))

  } else {

    return(wrap(3L, State))

  }

}

#' @export
print.slurm_status <- function(x, ...) {

  cat("Status: ", attr(x, "description"), " (Code ", x, ")\n", sep="")

  anames <- setdiff(names(attributes(x)), c("class", "description", "njobs"))

  if (attr(x, "njobs") > 1L) {
    cat("This is a job array. The status of each job, by array id, is the following:\n")
    for (i in anames) {
      l <- paste0(paste(attr(x, i), collapse = ", "), ".")
      if (l == ".")
        l <- " - "
      cat(sprintf(" %-10s: %s\n", i, l))
    }
  }
  invisible(x)

}

#' @export
#' @rdname status
#' @param name Character scalar. List of status to retrieve. This can be any of
#' `"done"`, `"failed"`, `"running"`, or `"pending"`.
`$.slurm_status` <- function(x, name) {

  if (name %in% c("done", "failed", "running", "pending")) {

    attr(x, which=name)

  } else
    stop("No status named \"", name, "\".", call. = FALSE)

}
