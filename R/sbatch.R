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
  if (!is.na(get_job_id(x)) && (status(x) %in% 1:2))
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

  # Keep the task IDs for this particular submission. This can be a subset of
  # x$njobs when a job is being resumed with, for example, `array = "2,4-6"`.
  submitted_ids <- submitted_array_ids(tmp_opts, x$njobs, x$opts_job)

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

    get_job_id(x) <- parse_sbatch_jobid(ans)
    x$array_ids <- submitted_ids

    # The cluster (if any) is needed to query/cancel the job later on
    x$cluster <- parse_sbatch_cluster(ans)

    # We need to update the job file and the latest submitted job
    LAST_SUBMITTED_JOB$set(x)
    write_slurm_job(x, snames("job", tmp_path = tmp_path, job_name = job_name))

  } else {
    get_job_id(x) <- NA_integer_
    x$cluster     <- NULL
  }

  if (wait)
    wait_slurm(x)

  # Not necesary
  invisible(x)

}

#' Array task IDs selected by an sbatch call
#'
#' The batch script generated by slurmR selects `1-njobs`. An `array` (or `a`)
#' command-line option overrides that selection for a resubmission.
#' @noRd
submitted_array_ids <- function(opts, njobs, script_opts = list()) {

  selection <- if (!is.null(opts$array)) opts$array else opts$a
  if (is.null(selection))
    selection <- if (!is.null(script_opts$array))
      script_opts$array
    else
      script_opts$a

  if (is.null(selection))
    return(seq_len(njobs))

  selection <- as.character(selection)[1L]
  selection <- sub("%[[:digit:]]+$", "", selection)
  selection <- sub("^\\[", "", selection)
  selection <- sub("\\]$", "", selection)

  expanded <- expand_array_indexes(paste0("0_[", selection, "]"))
  as.integer(sub("^[^_]+_", "", expanded))

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
  SBATCH   <- normalize_job_name(read_sbatch(x))
  job_name <- SBATCH[["job-name"]]
  if (!is.character(job_name))
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

  jobid   <- NA_integer_
  cluster <- NULL
  if (!opts_slurmR$get_debug()) {
    jobid   <- parse_sbatch_jobid(ans)
    cluster <- parse_sbatch_cluster(ans)
  }

  if (wait)
    wait_slurm_impl(jobid, clusters = cluster)

  # Jobs sent to another cluster carry its name (needed to query/cancel them)
  if (!is.null(cluster))
    attr(jobid, "cluster") <- cluster

  invisible(jobid)

}

#' Extract the job id and the cluster from the output of `sbatch`
#'
#' `sbatch` reports a successful submission with `Submitted batch job <id>`.
#' When the job is sent to another cluster (`-M`/`--clusters`, e.g., federated
#' or multi-cluster setups), Slurm appends ` on cluster <name>` to the message.
#' Older versions only print the first form. Since `stderr` is captured along
#' with `stdout`, the message may also be preceded by other lines (warnings,
#' retries) or by an `sbatch: ` prefix, so we look for the message in any line
#' instead of assuming its position.
#'
#' @param ans Character vector. Output captured from `sbatch`.
#' @return `parse_sbatch_jobid` returns an integer scalar with the job id. It
#' stops with an informative error if no job id can be found.
#' `parse_sbatch_cluster` returns the name of the cluster, or `NULL` if the
#' message does not include one.
#' @noRd
parse_sbatch_jobid <- function(ans) {

  lines <- ans[grepl(SBATCH_SUBMITTED, ans)]

  if (length(lines)) {
    jobid <- suppressWarnings(
      as.integer(sub(SBATCH_SUBMITTED, "\\2", lines[1L]))
    )
    if (!is.na(jobid))
      return(jobid)
  }

  stop(
    "Unable to parse the job id from the output of sbatch. The output was:\n",
    paste(ans, collapse = "\n"),
    call. = FALSE
  )

}

#' @noRd
parse_sbatch_cluster <- function(ans) {

  lines <- ans[grepl(SBATCH_SUBMITTED, ans)]

  if (!length(lines))
    return(NULL)

  cluster <- sub(SBATCH_SUBMITTED, "\\4", lines[1L])

  if (nzchar(cluster)) cluster else NULL

}

# Groups: 2 = job id, 4 = cluster name (only for cross-cluster submissions)
SBATCH_SUBMITTED <- paste0(
  "^\\s*(sbatch:\\s+)?Submitted batch job\\s+([[:digit:]]+)",
  "(\\s+on cluster\\s+(\\S+))?(\\s.*)?$"
)
