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
  if (!is.na(get_job_id(x)) && (status(get_job_id(x)) %in% 1:2))
    stop(
      "Job ", get_job_id(x)," is on the queue. If you wish to resubmit, ",
      "you first need to cancel the job by calling `scancel()`.",
      call. = FALSE
    )

  # Preparing options
  option   <- x$bashfile
  tmp_opts <- list(...)

  # Adding default options
  tmp_opts <- coalesce_slurm_options(tmp_opts)

  # Getting the tmp path and job name
  tmp_path <- get_tmp_path(x)
  job_name <- get_job_name(x)

  if (!opts_slurmR$get_debug()) {
    option <- c(parse_flags(tmp_opts), option)
  } else {
    option <- c(
      option,
      paste(
        ">",
        snames("out", tmp_path = tmp_path, job_name = job_name),
        ifelse(wait, "", "&")
      )
    )
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
    ans <- silent_system2(opts_slurmR$get_cmd(), option, stdout = TRUE, stderr = TRUE)
    cat(ans, sep = "\n")

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

    # Sometines it comes with infomration
    ans <- ans[grepl("^Submitted batch", ans)]

    get_job_id(x) <- as.integer(gsub(pattern = ".+ (?=[[:digit:]]+$)", "", ans, perl=TRUE))

    # We need to update the job file and the latest submitted job
    LAST_SUBMITTED_JOB$set(x)
    write_slurm_job(x, snames("job", tmp_path = tmp_path, job_name = job_name))

  } else
    get_job_id(x) <- NA_integer_

  if (wait)
    wait_slurm(x)

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
    job_name <- basename(x)

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
  ans <- silent_system2(opts_slurmR$get_cmd(), option, stdout = TRUE, stderr = TRUE)
  cat(ans, sep = "\n")

  # Sometines it comes with infomration
  ans <- ans[grepl("^Submitted batch", ans)]

  jobid <- as.integer(gsub(pattern = ".+ (?=[[:digit:]]+$)", "", ans, perl=TRUE))

  if (wait)
    wait_slurm(jobid)

  invisible(jobid)

}
