#' Utility function
#' If the job folder doesn't exists, this creates it.
#' @noRd
check_path <- function() {

  # Path specification
  path <- sprintf(
    "%s/%s",
    opts_sluRm$get_chdir(), opts_sluRm$get_job_name()
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
#' # -a=1 -b --hola=2 -y="I have spaces" --ms=2 --cpus-per-task=4
#' @export
#' @family Utility
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

  option <- ifelse(
    nchar(names(x)) > 1,
    paste0("--", names(x)),
    paste0("-", names(x))
  )

  vals <- character(length(option))
  for (i in seq_along(x)) {
    if (is.logical(x[[i]]) && !x[[i]])
      option[i] <- ""
    else if (!is.logical(x[[i]]) && !is.character(x[[i]]))
      vals[i] <- paste0("=", x[[i]])
    else if (is.character(x[[i]])) {
      if (grepl("\\s+", x[[i]]))
        vals[i] <- sprintf("=\"%s\"", x[[i]])
      else
        vals[i] <- sprintf("=%s", x[[i]])

    }
  }

  sprintf("%s%s", option, vals)
}

#' Full path names for Slurm jobs
#'
#' Using [opts_sluRm]`$get_chdir` and [opts_sluRm]`$get_job_name` creates
#' file names with full path to the objects. This function is intended for
#' internal use only.
#'
#' @param type can be any of r, sh, out, or rds.
#' @param array_id Integer. ID of the array to create the name.
#' @family Utility
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
    opts_sluRm$get_chdir(),
    opts_sluRm$get_job_name(),
    type
  )

}

#' Check the State (status) of a Slurm JOB
#'
#' @param x Either a Job id, or an object of class `slurm_job`.
#' @return An integer with an attribute `sacct`
#'
#' Codes:
#' - `-1`: Job not found. This may be a false negative since Slurm may still
#'   be scheduling the job.
#'
#' - `0`: Job completed. In this case all the components of the job
#'   have finalized without any errors.
#'
#' - `1`: Some jobs are still running/pending.
#'
#' - `2`: One or more jobs have failed.
#'
#' @export
#' @family Utility
state <- function(x) UseMethod("state")

#' @export
#' @rdname state
state.slurm_job <- function(x) state.default(x$jobid)

#' @export
#' @rdname state
JOB_STATE_CODES <- list(
  done    = "COMPLETED",
  failed  = c("BOOT_FAIL", "CANCELLED", "DEADLINE", "FAILED", "NODE_FAIL",
              "OUT_OF_MEMORY", "PREEMPTED", "REVOKED", "TIMEOUT"),
  running = c("RUNNING"),
  pending = c("PENDING", "REQUEUED", "RESIZING", "SUSPENDED")
)

#' @export
#' @rdname state
state.default <- function(x) {

  wrap <- function(val, S) do.call(structure, c(list(.Data = val), S))

  # Checking the data
  dat <- sacct(x)

  if (!nrow(dat))
    return(wrap(-1, NULL))

  # How many are done?
  JobID <- dat$JobID
  which_rows <- grepl("^[0-9]+([_][0-9]+)?$", JobID)
  JobID <- JobID[which_rows]
  JobID <- as.integer(gsub(".+[_]", "", JobID))
  State <- dat$State[which_rows]
  njobs <- length(State)
  State <- lapply(JOB_STATE_CODES, function(jsc) JobID[which(State %in% jsc)])

  if (length(State$done) == njobs)
    return(wrap(0, State))
  else if (length(State$failed) == 0)
    return(wrap(1, State))
  else
    return(wrap(2, State))


}

#' A wrapper of [Sys.getenv]
#'
#' This function is used within the R script written by `sluRm` to get the
#' current value of `SLURM_ARRAY_TASK_ID`, an environment variable that Slurm
#' creates when running an array. In the case that `opts_sluRm$get_debug() == TRUE`,
#' the function will return a 1 (see [opts_sluRm]).
#'
#' @param x Character scalar. Environment variable to get.
#'
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
Slurm_clean <- function(x) {

  # Checking if the job is running
  s <- if (opts_sluRm$get_debug() | !slurm_available()) 0
    else state(x)

  if (s == 1)
    stop("Some jobs are still running/pending (",
         paste(attr(s, "pending"), collapse=", "), ".", call. = FALSE)

  # Path specification
  path <- sprintf(
    "%s/%s",
    opts_sluRm$get_chdir(), opts_sluRm$get_job_name()
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
WhoAmI <- function() {

  vars <- c(
    "SLURM_LOCALID",
    "SLURMD_NODENAME",
    "SLURM_ARRAY_TASK_ID",
    "SLURM_CLUSTER_NAME",
    "SLURM_JOB_PARTITION",
    "SLURM_TASK_PID"
    )

  structure(sapply(vars, Sys.getenv), names = vars)

}

#' Check for possible actions for a `slurm_job` wrapper
#'
#' Users can choose whether to submit the job or not, to wait for it, and whether
#' they want to collect the results right away after the job has finished. This
#' function will help developers to figure out what set of actions need to be
#' taken depending on the plan.
#'
#' @param plan A character scalar with either of the following values:
#' `"collect"`, `"wait"`, `"submit"`, or `"none"`.
#' @details
#' This is a helper function that returns a list with three logical values:
#' `wait`, `collect`, and `submit`. There are four possible cases:
#'
#' - `plan == "collect"`, then all three are `TRUE`.
#' - `plan == "wait"`, then all but `collect` are `TRUE`.
#' - `plan == "submit"` then only `submit` equals `TRUE`.
#' - `plan == "none"` then all three are `FALSE`.
#'
#' In general, bot `wait` and `submit` will be passed to [sbatch].
#'
#' When `collect == TRUE`, then it usually means that the function will be calling
#' [Slurm_collect] right after submitting the job via [sbatch].
#'
#' @return A list with three logical scalars.
#'
#'
#' @export
the_plan <- function(plan) {

  if (length(plan) > 1L)
    stop("The `plan` must be a character of length 1.", call. = FALSE)
  if (!is.character(plan))
    stop("The `plan` must be a character of length 1.", call. = FALSE)

  PLAN <- c("collect", "wait", "submit", "none")
  if (!(plan %in% PLAN))
    stop("The `plan` must be either of: \"",
         paste0(PLAN, collapse = "\", \""), "\". Right now is: \"",
         plan, "\".", call. = FALSE)

  res <- list()
  if (plan == "collect") {

    res$collect <- TRUE
    res$wait    <- TRUE
    res$submit  <- TRUE

  } else if (plan == "wait") {

    res$collect <- FALSE
    res$wait    <- TRUE
    res$submit  <- TRUE

  } else if (plan == "submit") {

    res$collect <- FALSE
    res$wait    <- FALSE
    res$submit  <- TRUE

  } else if (plan == "none") {

    res$collect <- FALSE
    res$wait    <- FALSE
    res$submit  <- FALSE

  }

  return(res)

}
