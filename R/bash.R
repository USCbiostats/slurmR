#' @details The function `slurm_available` checks whether Slurm is available in
#' the system or not. It is usually called before calling any bash wrapper.
#' If available, the function will return `TRUE`, otherwise `FALSE`.
#'
#' @examples
#' # Are we under a Slurm Cluster?
#' slurm_available()
#'
#' @export
#' @rdname sbatch
slurm_available <- function() {

  if (.Platform$OS.type != "unix")
    return(FALSE)

  x <- tryCatch(
    system2("sbatch", "--version", stderr = TRUE, stdout = TRUE),
    error = function(e) e)

  if (inherits(x, "error"))
    FALSE
  else
    TRUE
}

#' Returns with an error if Slurm is not available
#' @noRd
stopifnot_slurm <- function() {

  if (!slurm_available())
    stop(
      "Slurm is not available on this system. If you are trying to debug or ",
      "run some tests, you should set the debug mode on (see opts_slurmR).",
      call. = FALSE
      )

}

stopifnot_submitted <- function(x) {

  if (is.na(x)) {
    stop(
      "This job hasn't started/been submitted yet. Nothing to do.",
      call. = FALSE
    )
  }

}

check_error <- function(cmd, ans) {

  if (length(attr(ans, "status")) && (attr(ans, "status") != 0)) {
    stop(
      "An error has occurred when calling `", cmd,"`:\n",
      paste(ans, collapse="\n"), call. = FALSE)
  }

}

silent_system2 <- function(...) {

  fun_name <- as.character(sys.call()[[1]])

  ans <- suppressWarnings({
    tryCatch(system2(...), error = function(e) e)
  })

  check_error(fun_name, ans)
  ans

}

#' This environment sets and gets the latest submitted job. The function
#' [last_submitted_job] is a wrapper of it visible to the user.
#' @noRd
LAST_SUBMITTED_JOB <- (function() {

  record <- new.env(parent = emptyenv())

  record$job <- NULL
  record$set <- function(job) {

    if (!inherits(job, "slurm_job"))
      stop("The `job` argument must be an object of class `slurm_job`.",
           call. = FALSE)

    record$job <- job

    invisible()
  }
  record$get <- function() {
    record$job
  }

  return(record)

})()

#' @rdname slurm_job
#' @export
#' @details The `las_submitted_job` function will return the latest `slurm_job`
#' object that was submitted via [sbatch] in the current session. The `last_job`
#' function is just an alias of the later. If no job has been submitted, then
#' the resulting value will be `NULL`.
#' @examples
#' \dontrun{
#' # The last_job function can be handy when `plan = "collect"` in a called,
#' # for example
#' job <- Slurm_lapply(1:1000, function(i) runif(100), njobs = 2, plan = "collect")
#'
#' # Post collection analysis
#' status(last_job())
#' }
last_submitted_job <- function() {

  LAST_SUBMITTED_JOB$get()

}

#' @export
#' @rdname slurm_job
last_job <- last_submitted_job

#' R wrappers for *Slurm* commands
#'
#' The functions `sbatch`, `scancel`, `squeue`, `sacct`, and `slurm.conf` are
#' wrappers of calls to Slurm functions via [system2].
#'
#' @param x Either an object of class `slurm_job`, or, in some cases, an
#' integer as a Slurm jobid. Note that some functions allow passing no arguments.
#' @param wait Logical scalar. When `TRUE` the function will pass the `--wait`
#' flag to `Slurm` and set `wait = TRUE` in the [system2] call.
#' @param submit Logical, when `TRUE` calls [sbatch] to submit the job to slurm.
#' @param ... Further flags passed to the command line function.
#'
#' @details
#' In the case of `sbatch`, function takes an object of class `slurm_job` and
#' submits it to the queue. In debug mode the job will be submitted via `sh`
#' instead.
#'
#' @return In the case of `sbatch`, depends on what `x` is:
#'
#' - If `x` is of class [slurm_job], then it returns the same object including
#'   the Slurm job ID (if the job was submitted to the queue).
#'
#' - If `x` is a file path (e.g. a bash script), an integer with the jobid number
#'   (again, if the job was submitted to Slurm).
#'
#' The functions `squeue` and `sacct` return a data frame with the information
#' returned by the command line utilities. The function `scancel` returns NULL.
#'
#' `slurm_available()` returns a logical scalar equal to `TRUE` if Slurm is
#' available.
#'
#' `slurm.conf()` and `SchedulerParameters()` return information about the
#' Slurm cluster, if available.
#'
#' @export
#' @aliases submit
sbatch <- function(x, wait = FALSE, submit = TRUE, ...) UseMethod("sbatch")

hline <- function(..., sep="\n") {
  message(paste(rep("-", options("width")), collapse=""))
  message(paste(..., collapse = "\n"))
  message(paste(rep("-", options("width")), collapse=""))
}

#' @export
#' @rdname sbatch
#' @examples
#' \dontrun{
#' # Submitting a simple job
#' job <- Slurm_EvalQ(slurmR::WhoAmI(), njobs = 4L, plan = "submit")
#'
#' # Checking the status of the job (we can simply print)
#' job
#' status(job) # or use the state function
#' sacct(job) # or get more info with the sactt wrapper.
#'
#' # Suppose one of the jobs is taking too long to complete (say #4)
#' # we can stop it and resubmit the job as follows:
#' scancel(job)
#'
#' # Resubmitting only 4
#' sbatch(job, array = 4) # A new jobid will be assigned
#'
#' }
sbatch.slurm_job <- function(x, wait = FALSE, submit = TRUE, ...) {

  # Checking the status of the job. If the job exists and it is completed,
  # then we can resubmit, otherwise, the user must actively cancell it
  if (!is.na(x$jobid) && (status(x$jobid) %in% 1:2))
      stop(
        "Job ", x$jobid," is on the queue. If you wish to resubmit, ",
        "you first need to cancel the job by calling `scancel()`.",
        call. = FALSE
        )

  # Preparing options
  option   <- x$bashfile
  tmp_opts <- list(...)

  # Adding default options
  tmp_opts <- coalesce_slurm_options(tmp_opts)

    if (!opts_slurmR$get_debug()) {
    option <- c(parse_flags(tmp_opts), option)
  } else {
    option <- c(option, paste(">", snames("out"), ifelse(wait, "", "&")))
  }

  if (opts_slurmR$get_verbose()) {
    hline(
      "[VERBOSE MODE ON] The R script that will be used is located at:",
      x$rscript, "and has the following contents:"
      )
    message(paste(readLines(x$rscript), collapse = "\n"))
    hline(
      "The bash file that will be used is located at:",
      x$bashfile,
      "and has the following contents:"
      )
    message(paste(readLines(x$bashfile), collapse = "\n"))
    hline("EOF")
  }

  if (submit) {

    # If the command used to execute the job is slurm, then this is an error.
    if (opts_slurmR$get_cmd() == "sbatch")
      stopifnot_slurm()

    message("Submitting job...", appendLF = FALSE)
    ans <- silent_system2(opts_slurmR$get_cmd(), option, stdout = TRUE, wait=TRUE)

  } else {
    warning(
      "[submit = FALSE] The job hasn't been submitted yet.",
      " Use sbatch() to submit the job, or you can submit it via command line",
      " using the following:\n", paste(
        opts_slurmR$get_cmd(),
        paste(option, collapse=" ")
        ),
      immediate. = TRUE,
      call.      = FALSE
      )
    return(x)
  }

  # Warning that the call has been made and storing the id
  if (!opts_slurmR$get_debug()) {

    x$jobid <- as.integer(gsub(pattern = ".+ (?=[0-9]+$)", "", ans, perl=TRUE))
    message(" jobid:", x$jobid, ".")

    # We need to update the job file and the latest submitted job
    LAST_SUBMITTED_JOB$set(x)
    write_slurm_job(x, snames("job"))

  } else
    x$jobid <- NA

  if (wait)
    wait_slurm(x$jobid)

  # Not necesary
  invisible(x)

}

#' @export
#' @details The method for character scalars is used to submit jobs using a slurm
#' script.
#' @rdname sbatch
sbatch.character <- function(x, wait = FALSE, submit = TRUE, ...) {

  # Checking whether this is a valid file
  if (!file.exists(x))
    stop(
      "No file named \"", x, "\". When character, `x` must be a file path.",
      call. = FALSE
      )

  # Capturing jobname
  SBATCH   <- read_sbatch(x)
  job_name <- SBATCH["job-name"]
  if (is.na(job_name))
    job_name <- gsub(".+[/](?=[^/]+$)", "", x, perl=TRUE)

  tmp_opts <- list(...)
  tmp_opts <- coalesce_slurm_options(tmp_opts)
  tmp_opts[["job-name"]] <- NULL

  option <- if (!opts_slurmR$get_debug())
   c(parse_flags(tmp_opts, `job-name` = job_name), x)
  else c(x, ifelse(wait, "", "&"))

  # Not submitting means that we just want the script
  if (!submit) {
    warning(
      "[submit = FALSE] The job hasn't been submitted yet.",
      " Use sbatch() to submit the job, or you can submit it via command line",
      " using the following:\n", paste(
        opts_slurmR$get_cmd(),
        paste(option, collapse=" ")
      ),
      immediate. = TRUE,
      call.      = FALSE
    )

    return(invisible(NULL))
  }

  if (opts_slurmR$get_verbose()) {
    hline(
      "[VERBOSE MODE ON] The bash file that will be used is located at:",
      x,
      "and has the following contents:"
    )
    message(paste(readLines(x), collapse = "\n"))
    hline("EOF")
  }

  # This will stop if slurm is not available
  if (!opts_slurmR$get_debug())
    stopifnot_slurm()

  # Submitting the job
  message("Submitting job...", appendLF = FALSE)
  ans <- silent_system2(opts_slurmR$get_cmd(), option, stdout = TRUE, wait=TRUE)

  jobid <- as.integer(gsub(pattern = ".+ (?=[0-9]+$)", "", ans, perl=TRUE))
  message(" jobid:", jobid, ".")

  if (wait)
    wait_slurm(jobid)

  invisible(jobid)

}

#' Waits for the `jobid` to be completed
#' @noRd
wait_slurm <- function(pid, freq = 0.1, force = TRUE) {

  # Checking if Slurm and debug mode
  if (opts_slurmR$get_debug()) {
    warning("waiting is not available in debug mode.", call. = FALSE)
    return()
  } else if (!slurm_available())
    stopifnot_slurm()

  while(TRUE) {

    # Getting status, every freq secs
    Sys.sleep(freq)
    s <- status(pid)

    # The job has not been found. If force = TRUE, then, if the job has not
    # been found, then just retry until it finds the job. Otherwise, if no
    # forcing, then break out if no job has been found.
    if (force && s == -1L) {
      next
    } else if (!force && s == -1L) {
      print(s)
      break
    }

    # Is it a job array?
    njobs <- attr(s, "njobs")
    if (njobs > 1L) {

      # End if either done or failed
      ncompleted <- length(attr(s, "failed")) + length(attr(s, "done"))
      if (ncompleted == njobs)
        break

    } else if (s %in% c(0L, 99L))
      # If it is not a job array (either failed or done)
      break

  }

  invisible(NULL)

}

#' @export
#' @rdname sbatch
#' @details The wrapper of squeue includes the flag `-o%all` which returns all
#' available fields separated by a vertical bar. This cannot be changed since it
#' is the easiest way of processing the information in R.
squeue <- function(x = NULL, ...) UseMethod("squeue")

#' @export
#' @rdname sbatch
squeue.default <- function(x = NULL, ...) {

  # Checking for slurm
  stopifnot_slurm()

  # Notice that the jobid may be null
  option <- c(sprintf("-j%i", x), "-o%all", parse_flags(...))

  # message("Submitting job...")
  ans <- silent_system2("squeue", option, stdout = TRUE, stderr = TRUE, wait = TRUE)

  # Parsing the data
  ans <- lapply(ans, strsplit, split="|", fixed=TRUE)
  ans <- do.call(rbind, lapply(ans, unlist))

  structure(
    as.data.frame(ans[-1, , drop=FALSE], stringsAsFactors=FALSE),
    names = ans[1,]
  )

}

#' @export
#' @rdname sbatch
squeue.slurm_job <- function(x, ...) {

  stopifnot_submitted(x$jobid)
  squeue.default(x$jobid, ...)

}

#' @export
#' @rdname sbatch
scancel <- function(x = NULL, ...) UseMethod("scancel")

#' @export
#' @rdname sbatch
scancel.default <- function(x = NULL, ...) {

  # Checking for slurm, and if passes, if it started
  stopifnot_slurm()

  # Preparing options
  option <- c(parse_flags(...), x)

  ans <- silent_system2("scancel", option, stdout = TRUE, stderr = TRUE)

  invisible()

}

#' @export
#' @rdname sbatch
scancel.slurm_job <- function(x = NULL, ...) {

  stopifnot_submitted(x$jobid)
  scancel.default(x$jobid, ...)

}

#' @rdname sbatch
#' @export
sacct <- function(x, ...) UseMethod("sacct")

#' @export
#' @param brief,parsable,allocations Logical. When `TRUE`, these are passed as flags directly
#' to the command line function `sacct`.
#' @rdname sbatch
sacct.default <- function(
  x           = NULL,
  brief       = TRUE,
  parsable    = TRUE,
  allocations = TRUE,
  ...
  ) {

  # Checking for slurm
  stopifnot_slurm()

  flags <- parse_flags(
    c(
      list(brief = brief, parsable = parsable, jobs = x, allocations = allocations),
      list(...)
      )
    )

  ans <- silent_system2("sacct", flags, stdout = TRUE)

  ans <- lapply(ans, strsplit, split="|", fixed=TRUE)
  ans <- do.call(rbind, lapply(ans, unlist))

  structure(
    as.data.frame(ans[-1, , drop=FALSE], stringsAsFactors=FALSE),
    names = ans[1,]
  )

}

#' @export
#' @rdname sbatch
sacct.slurm_job <- function(x, ...) {

  stopifnot_submitted(x$jobid)
  sacct.default(x$jobid, ...)

}


#' @export
#' @rdname sbatch
#' @details The function `slurm.conf` is a wrapper of the function `scontrol` that
#' returns configuration info about Slurm, in particular, the underlying command
#' that is called is `scontrol show conf`. This returns a named character vector
#' with configuration info about the cluster. The name of this function matches
#' the name of the file that holds this information.
#' @examples
#'
#' \dontrun{
#' # What is the maximum number of jobs (array size) that the system
#' # allows?
#' sconfig <- slurm.conf() # We first retrieve the info.
#' sconfig["MaxArraySize"]
#' }
#'
slurm.conf <- function() {

  stopifnot_slurm()
  conf <- silent_system2("scontrol", "show conf", stdout = TRUE, stderr = TRUE)

  conf <- conf[grepl("^[[:graph:]]+\\s*[=]", conf)]

  structure(
    gsub("^[[:graph:]]+\\s*[=]\\s*", "", conf),
    names = gsub("\\s*[=].+", "", conf)
  )

}

#' @export
#' @rdname sbatch
#' @details The function `SchedulerParameters` is just a wrapper of [slurm.conf].
#' It processes the field "SchedulerParameters" included in the configuration
#' file and has information relevant for the scheduler.
SchedulerParameters <- function() {

  conf <- slurm.conf()["SchedulerParameters"]
  conf <- strsplit(x = conf, split = ",", fixed = TRUE)[[1]]

  # Which are logicals
  logicals <- grepl("^[^=]+$", conf)

  values <- gsub("^.+[=]", "", conf)
  values[logicals] <- TRUE
  names(values) <- gsub("[=].+", "", conf)

  values <- as.list(values)
  values[] <- lapply(values, function(v) {
    if (all(grepl("^[0-9]+$", v)))
      as.integer(v)
    else if (all(v == "TRUE")) {
      rep(TRUE, length(v))
    } else
      v
      })

  return(as.list(values))
}
