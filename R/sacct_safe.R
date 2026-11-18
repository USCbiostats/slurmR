#' @export
#' @details `sacct.` is an alternative that works around when `sacct` fails due to
#' lack of accounting on. This function is not intended for direct call.
#' @param no_sacct Logical. Skip `sacct` directly (for internal use only.)
#' @rdname sbatch
sacct_ <- function(x = NULL, ..., no_sacct = FALSE) {

  if (!no_sacct) {

    ans <- tryCatch(sacct(x = x, ...), error = function(e) e)

    if (!inherits(ans, "error"))
      return(ans)

  }

  warning(
    "The call to -sacct- failed. This is probably due to not",
    " having slurm accounting up and running. For more information, ",
    "checkout this discussion: https://github.com/USCbiostats/slurmR/issues/29",
    call. = FALSE, immediate. = TRUE
    )

  # Checking if the job path exists
  if (is.null(x))
    stop("Since -sacct- failed, `x` cannot be NULL (it currently is).", call. = FALSE)

  # The fallback needs the slurm_job object (number of jobs, tmp path, and job
  # name). If only a job ID was passed, we can only recover it from the last
  # submitted job (see #29 and #40).
  if (!inherits(x, "slurm_job")) {

    job <- last_job()
    if (
      !is.null(job) && length(x) == 1L && !is.na(get_job_id(job)) &&
      isTRUE(as.character(get_job_id(job)) == as.character(x))
    ) {
      x <- job
    } else {
      stop(
        "Since -sacct- failed, the status of the job is inferred from its ",
        "output files, which requires an object of class `slurm_job`. Only ",
        "the job ID (", paste(x, collapse = ", "), ") was passed, and it does ",
        "not match the last submitted job. Try passing the `slurm_job` object ",
        "instead (see ?read_slurm_job).",
        call. = FALSE
      )
    }

  }

  # Getting coordinates
  tmp_path <- get_tmp_path(x)
  job_name <- get_job_name(x)
  job_path <- file.path(tmp_path, job_name)
  job_id   <- get_job_id(x)
  njobs    <- x$njobs

  # Finding file
  if (!dir.exists(job_path))
    stop("The path of the job does not exists: ",
         job_path, "\n.Have you deleted?", call. = FALSE)

  # Generating the output
  res <- data.frame(
    JobID = sprintf("%i_%i", job_id, seq_len(njobs)),
    State = NA_character_,
    ExitCode = "0:0",
    stringsAsFactors = FALSE
  )

  # Generating files to check
  out_files <- gsub("%(A|a)", "%s", snames("out", tmp_path = tmp_path, job_name = job_name))
  out_files <- sprintf(out_files, job_id, seq_len(njobs))
  rds_files <- snames(
    "rds",
    seq_len(njobs),
    tmp_path = tmp_path,
    job_name = job_name
  )

  # Trying to capture the jobarray
  array_state <- tryCatch(squeue(array = TRUE), error = function(e) e)

  # Going file by file
  for (i in seq_len(njobs)) {

    # Let's check if it finished
    if (file.exists(rds_files[i])) {

      res$State[i] <- "COMPLETED"
      next

    }

    # If it hasn't finished, then it could be that is running or pending
    # if nothing shows in the squeue, then it is because it failed
    if (inherits(array_state, "error")) {

      res$State[i] <- "FAILED"
      next

    }

    where_is <- which(grepl(paste0("_", i, "$"), array_state$JOBID))
    if (length(where_is)) {

      # Trying to catch the actual state
      res$State[i] <- array_state$STATUS[where_is]
      next

    }

    # Squeue running but this is not present, so failed
    res$State[i] <- "FAILED"
    next

  }

  return(res)

}
