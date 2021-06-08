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

  # Getting coordinates
  tmp_path <- get_tmp_path(x)
  job_name <- get_job_name(x)
  job_path <- file.path(tmp_path, job_name)
  job_id   <- get_job_id(x)

  # Finding file
  if (!dir.exists(job_path))
    stop("The path of the job does not exists: ",
         job_path, "\n.Have you deleted?", call. = FALSE)

  # Generating the output
  res <- data.frame(
    JobID = sprintf("%i_%i", job_id, 1:x$njobs),
    State = NA_character_,
    ExitCode = "0:0",
    stringsAsFactors = FALSE
  )

  # Generating files to check
  out_files <- gsub("%(A|a)", "%s", snames("out", tmp_path = tmp_path, job_name = job_name))
  out_files <- sprintf(out_files, job_id, 1:x$njobs)
  rds_files <- snames("rds", 1:x$njobs, tmp_path = tmp_path, job_name = job_name)

  # Trying to capture the jobarray
  array_state <- tryCatch(squeue(array = TRUE), error = function(e) e)

  # Going file by file
  for (i in 1:x$njobs) {

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
