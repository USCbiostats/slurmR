#' Check the status of a Slurm JOB
#'
#' Using the [sacct] function, it checks the status of a particular job and
#' returns information about its current state, with details regarding the
#' jobs (if an array) that are done, running, pending, or failed.
#'
#' @param x Either a Job id, an object of class `slurm_job`, or an object of
#' class `slurm_status`.
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
status <- function(x) UseMethod("status")

#' @export
#' @rdname status
status.slurm_job <- function(x) {
  status.default(get_job_id(x))
}

#' @export
#' @rdname status
status.default <- function(x) {

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

  dat <- sacct(x, brief = TRUE, parsable = TRUE, allocations = TRUE)

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

  STATE_CODES <- split(JOB_STATE_CODES$name, JOB_STATE_CODES$type)


  # If it is an array (multiple rows)
  if (nrow(dat) > 1L) {
    # Getting the array id
    JobID <- as.integer(gsub(".+[_]", "", JobID))
  }

  njobs <- length(State)
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
