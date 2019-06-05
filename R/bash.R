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

  x <- tryCatch(
    system2("type", "sbatch", stderr = TRUE, stdout = TRUE),
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
    stop("Slurm is not available on this system.", call. = FALSE)

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

#' @rdname sbatch
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
#' state(last_job())
#' }
last_submitted_job <- function() {

  LAST_SUBMITTED_JOB$get()

}

#' @export
#' @rdname slurm_job
last_job <- last_submitted_job

#' R wrappers for *Slurm* commands
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
#' @export
#' @aliases submit
sbatch <- function(x, wait = FALSE, submit = TRUE, ...) UseMethod("sbatch")

hline <- function(..., sep="\n") {
  cat("\n",rep("-", options("width")), "\n",sep="")
  do.call(cat, c(list(...), sep=sep))
  cat(rep("-", options("width")), "\n",sep="")
}

#' @export
#' @rdname sbatch
#' @examples
#' \dontrun{
#' # Submitting a simple job
#' job <- Slurm_EvalQ(sluRm::WhoAmI(), njobs = 4L, plan = "submit")
#'
#' # Checking the status of the job (we can simply print)
#' job
#' state(job) # or use the state function
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
  if (!is.na(x$jobid) && (state(x$jobid) == 1L))
      stop(
        "Job ", x$jobid," is already running. If you wish to resubmit, ",
        "you first need to cancel the job by calling `scancel()`.",
        call. = FALSE
        )

  # Preparing options
  option <- x$bashfile

  if (!opts_sluRm$get_debug()) {
    option <- c(parse_flags(c(x$job_opts,...)), option)
  } else {
    option <- c(option, paste(">", snames("out"), ifelse(wait, "", "&")))
  }

  if (opts_sluRm$get_verbose()) {
    hline(
      "[VERBOSE MODE ON] The R script that will be used is located at:",
      x$rscript, "and has the following contents:"
      )
    cat(readLines(x$rscript), sep = "\n")
    hline(
      "The bash file that will be used is located at:",
      x$bashfile,
      "and has the following contents:"
      )
    cat(readLines(x$bashfile), sep = "\n")
    hline("EOF")
  }

  if (submit) {

    # If the command used to execute the job is slurm, then this is an error.
    if (opts_sluRm$get_cmd() == "sbatch")
      stopifnot_slurm()

    message("Submitting job...", appendLF = FALSE)
    ans <- silent_system2(opts_sluRm$get_cmd(), option, stdout = TRUE, wait=TRUE)

  } else {
    warning(
      "[submit = FALSE] The job hasn't been submitted yet.",
      " Use sbatch() to submit the job, or you can submit it via command line",
      " using the following:\n", paste(
        opts_sluRm$get_cmd(),
        paste(option, collapse=" ")
        ),
      immediate. = TRUE,
      call.      = FALSE
      )
    return(x)
  }

  # Warning that the call has been made and storing the id
  if (!opts_sluRm$get_debug()) {

    x$jobid <-as.integer(gsub(pattern = ".+ (?=[0-9]+$)", "", ans, perl=TRUE))
    message(" jobid:", x$jobid, ".")

    # We need to update the job file and the latest submitted job
    LAST_SUBMITTED_JOB$set(x)
    write_slurm_job(x, snames("job"))

  } else
    x$jobid <- NA

  if (wait) {

    ans <- sbatch_dummy(
      `job-name` = paste0(x$job_opts$`job-name`, "-dummy"),
      dependency = paste0("afterany:", x$jobid),
      partition  = x$job_opts$partition,
      account    = x$job_opts$account
      )
    check_error("srun", ans)

  }

  # Not necesary
  invisible(x)

}

#' Waits for the `jobid` to be completed
#' @noRd
sbatch_dummy <- function(...) {

  if (opts_sluRm$get_debug()) {
    warning("Waiting is not available in debug mode.", call.=FALSE)
    return(0)
  }

  message("Waiting for the job to be done...", appendLF = FALSE)

  flags <- parse_flags(
    c(
      list(
        output = "/dev/null",
        quiet  = TRUE,
        wait   = TRUE
        ),
      ...)
    )

  # Checking for slurm
  stopifnot_slurm()

  # Dummy file to run sbatch
  tmp <- tempfile(fileext = ".sbatch")
  writeLines("#!/bin/sh\n\necho 0", tmp)

  cmd <- sprintf("%s %s", paste(flags, collapse=" "), tmp)
  ans <- silent_system2("sbatch", cmd, wait = TRUE, stdout=TRUE)

  message("Done.")

  structure(ans, cmd = paste("srun", cmd))


}


#' @export
#' @rdname sbatch
squeue <- function(x = NULL, ...) UseMethod("squeue")

#' @export
#' @rdname sbatch
squeue.default <- function(x = NULL, ...) {

  # Checking for slurm
  stopifnot_slurm()

  # Notice that the jobid may be null
  option <- c(sprintf("-j%i", x), parse_flags(...))

  # message("Submitting job...")
  ans <- silent_system2("squeue", option, stdout=TRUE, stderr = TRUE, wait=TRUE)

  ans

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
#' @param brief,parsable Logical. When `TRUE`, these are passed as flags directly
#' to the command line function `sacct`.
#' @rdname sbatch
sacct.default <- function(x, brief=TRUE, parsable = TRUE, ...) {

  # Checking for slurm
  stopifnot_slurm()

  flags <- parse_flags(c(list(brief=brief, parsable=parsable), list(...)))
  flags <- c(flags, paste0("--jobs=", x))

  ans <- silent_system2("sacct", flags, stdout = TRUE)

  ans <- lapply(ans, strsplit, split="|", fixed=TRUE)
  ans <- do.call(rbind, lapply(ans, unlist))

  structure(
    as.data.frame(ans[-1,], stringsAsFactors=FALSE),
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
#' returns configuration info about Slurm, in particular, the underlaying command
#' that is called is `scontrol show conf`. This returns a named character vector
#' with configuration info about the cluster. The name of this function matches
#' the name of the file that holds this information.
#' @examples
#'
#' \dontrun{
#  # What is the maximum number of jobs (array size) that the system
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
