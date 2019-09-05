#' Utility function
#' If the job folder doesn't exists, this creates it.
#' @noRd
check_path <- function() {

  # Path specification
  path <- sprintf(
    "%s/%s",
    opts_sluRm$get_tmp_path(), opts_sluRm$get_job_name()
    )

  # The thing
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE)

  invisible()
}



#' Utility function
#' @param ... Options to be parsed as bash flags.
#' @examples
#' cat(parse_flags(a=1, b=TRUE, hola=2, y="I have spaces", ms=2, `cpus-per-task`=4))
#' # -a 1 -b --hola=2 -y "I have spaces" --ms=2 --cpus-per-task=4
#' @export
#' @family utilities
parse_flags <- function(...) UseMethod("parse_flags")

#' @export
#' @rdname parse_flags
parse_flags.default <- function(...) {
  parse_flags.list(list(...))
}

#' @export
#' @param x A named list.
#' @rdname parse_flags
parse_flags.list <- function(x, ...) {

  # Skipping NULL and NAs
  if (length(x))
    x <- x[which(sapply(x, function(z) (length(z) > 0) && !is.na(z) ))]

  # If no flags are passed, then return ""
  if (!length(x))
    return("")

  single_char <- nchar(names(x)) == 1

  option <- ifelse(
    single_char,
    paste0("-", names(x)),
    paste0("--", names(x))
  )

  vals <- character(length(option))
  for (i in seq_along(x)) {

    # Includes equal
    equal_sign <- ifelse(single_char[i], " ", "=")

    if (is.logical(x[[i]]) && !x[[i]])
      option[i] <- ""
    else if (!is.logical(x[[i]]) && !is.character(x[[i]]))
      vals[i] <- paste0(equal_sign, x[[i]])
    else if (is.character(x[[i]])) {
      if (grepl("\\s+", x[[i]]))
        vals[i] <- sprintf("%s\"%s\"", equal_sign, x[[i]])
      else
        vals[i] <- sprintf("%s%s", equal_sign, x[[i]])

    }
  }

  sprintf("%s%s", option, vals)
}

#' Full path names for Slurm jobs
#'
#' Using [opts_sluRm]`$get_tmp_path` and [opts_sluRm]`$get_job_name` creates
#' file names with full path to the objects. This function is intended for
#' internal use only.
#'
#' @param type can be any of r, sh, out, or rds.
#' @param array_id Integer. ID of the array to create the name.
#' @family utilities
#' @export
snames <- function(type, array_id) {

  # Checks if the folder exists
  check_path()

  if (!missing(array_id) && length(array_id) > 1)
    return(sapply(array_id, snames, type = type))

  type <- switch (
    type,
    r   = "00-rscript.r",
    sh  = "01-bash.sh",
    out = "02-output-%A-%a.out",
    rds = if (missing(array_id))
      "03-answer-%03i.rds"
    else sprintf("03-answer-%03i.rds", array_id),
    job = "job.rds",
    stop(
      "Invalid type, the only valid types are `r`, `sh`, `out`, and `rds`.",
      call. = FALSE
    )
  )

  sprintf(
    "%s/%s/%s",
    opts_sluRm$get_tmp_path(),
    opts_sluRm$get_job_name(),
    type
  )

}

#' Check the status of a Slurm JOB
#'
#' Using the [sacct] function, it checks the status of a particular error and
#' returns information about its current state.
#'
#' @param x Either a Job id, or an object of class `slurm_job`.
#'
#' @return An integer with attributes. The attributes are integer vectors indicating
#' which jobs fail in the categories of `completed`, `failed`, and `pending` (see
#' [JOB_STATE_CODES]). Possible return values are
#'
#' - `-1`: Job not found. This may be a false negative since Slurm may still
#'   be scheduling the job.
#'
#' - `0`: Job completed. In this case all the components of the job
#'   have finalized without any errors.
#'
#' - `1`: All jobs are pending resource allocation.
#'
#' - `2`: One or more jobs are still running.
#'
#' - `99`: One or more jobs have failed.
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
#' status(x)
#' # A possible result: An integer with attributes
#' # [1] 1
#' # attr(,"pending")
#' # [1] 1
#' # attr(,"failed")
#' # integer(0)
#' # attr(,"done")
#' # [1] 2
#'
#'
#' }
status <- function(x) UseMethod("status")

#' @export
#' @rdname status
status.slurm_job <- function(x) {
  status.default(x$jobid)
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
      `2`  = "One or more jobs are still running.",
      `99` = "One or more jobs failed."
      )

    do.call(structure, c(list(.Data = val, description=desc, class="slurm_status"), S))
  }

  # Checking the data
  if (is.na(x))
    return(wrap(-1L, NULL))

  dat <- sacct(x)

  # We only need to keep the main line of the account
  if (!nrow(dat))
    return(wrap(-1L, NULL))

  # Processing ids
  dat  <- dat[grepl("^[^\\.]+$", dat$JobID), , drop=FALSE]
  dat. <- NULL
  for (i in 1L:nrow(dat)) {

    # Expanding Array indexes, and the data retrieved from sacct.
    idx <- expand_array_indexes(dat$JobID[i])

    if (i == 1L) {
      dat. <- cbind(dat[i, , drop=FALSE], NewId = idx)
    } else {

      dat. <- rbind(
        dat.,
        cbind(dat[i, , drop=FALSE], NewId = idx),
        stringsAsFactors = FALSE,
        make.row.names   = FALSE
      )

    }

  }

  dat <- subset(dat., select = c(-JobID))
  rownames(dat) <- seq_len(nrow(dat))
  colnames(dat)[ncol(dat)] <- "JobID"

  # Filtering the data, we don't use the steps, just the jobs.
  JobID <- dat$JobID
  State <- gsub("\\s+.+", "", dat$State)

  STATE_CODES <- split(JOB_STATE_CODES$name, JOB_STATE_CODES$type)

  # If it is an array (multiple rows)
  if (nrow(dat) > 1L) {
    # Getting the array id
    JobID <- as.integer(gsub(".+[_]", "", JobID))
  }

  njobs <- length(State)
  State <- lapply(STATE_CODES, function(jsc) JobID[which(State %in% jsc)])

  if (length(State$done) == njobs) {

    return(wrap(0L, State))

  } else if (length(State$pending) == njobs) {

    return(wrap(1L, State))

  } else if (length(State$failed) > 0L) {

    return(wrap(99L, State))

  } else {

    return(wrap(2L, State))

  }

}

#' @export
print.slurm_status <- function(x, ...) {

  cat("Current status: ", x, " (")
  cat(attr(x, "description"), ")\n")

  invisible(x)

}

#' A wrapper of [Sys.getenv]
#'
#' This function is used within the R script written by `sluRm` to get the
#' current value of `SLURM_ARRAY_TASK_ID`, an environment variable that Slurm
#' creates when running an array. In the case that `opts_sluRm$get_debug() == TRUE`,
#' the function will return a 1 (see [opts_sluRm]).
#'
#' @param x Character scalar. Environment variable to get.
#' @family utilities
#' @export
Slurm_env <- function(x) {

  y <- Sys.getenv(x)

  if ((x == "SLURM_ARRAY_TASK_ID") && y == "") {
    return(1)
  }

  y

}

#' Clean a session.
#' @param x An object of class `slurm_job`.
#' @export
#' @family post submission
#' @family utilities
Slurm_clean <- function(x) {

  # Checking if the job is running
  s <- if (opts_sluRm$get_debug() | !slurm_available()) 0
    else status(x)

  if (s %in% 1L:2L)
    stop("Some jobs are still running/pending (",
         paste(attr(s, "pending"), collapse=", "), ".", call. = FALSE)

  # Path specification
  path <- sprintf(
    "%s/%s",
    opts_sluRm$get_tmp_path(), opts_sluRm$get_job_name()
  )

  if (dir.exists(path))
    unlink(path, recursive = TRUE, force = TRUE)
  else
    invisible(0)

}

#' Information about where jobs are submitted
#'
#' This returns a named vector with the following variables:
#' \Sexpr{paste(names(sluRm::WhoAmI()), collapse = ", ")}
#' @export
#' @family utilities
WhoAmI <- function() {

  vars <- c(
    "SLURM_LOCALID",
    "SLURMD_NODENAME",
    "SLURM_ARRAY_TASK_ID",
    "SLURM_CLUSTER_NAME",
    "SLURM_JOB_PARTITION",
    "SLURM_TASK_PID"
    )


  ans <- structure(sapply(vars, Sys.getenv), names = vars)
  # I only do this b/c I may need to use this in other context
  if (!slurm_available() | opts_sluRm$get_debug()) {
    ans["SLURM_TASK_PID"] <- Sys.getpid()
    ans["SLURMD_NODENAME"] <- "localhost"
    ans["SLURM_ARRAY_TASK_ID"] <- 1
  }

  ans

}

#' @export
#' @rdname WhoAmI
#' @details `whoami` is just an alias of `WhoAmI`.
whoami <- WhoAmI

