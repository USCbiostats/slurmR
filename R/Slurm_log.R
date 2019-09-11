#' Check the R logfile of a job.
#'
#' After submission, the functions of type [Slurm_*apply][Slurm_lapply] generate
#' log files, one per each job in the job array. The `Slurm_log` function can be
#' used to check the log files of jobs in the array that failed.
#'
#' @param x An object of class [slurm_job].
#' @param cmd Character scalar. The name of the command to use to call view the
#' log file.
#' @param which. An integer scalar. The number of the array job to check. This
#' should range between 1 and `x$njobs`.
#'
#' @export
#' @family post submission
#' @family utilities
#' @examples
#' \dontrun{
#' x <- Slurm_EvalQ(sluRm::whoami(), plan = "wait")
#' Slurm_log(x) # Checking the R log
#' }
Slurm_log <- function(x, which. = NULL, cmd = "less") {

  if (!inherits(x, "slurm_job"))
    stop("`x` must be an object of class \"slurm_job\".", call. = FALSE)

  # We only execute this function if we are running in interactive mode!
  if (!interactive()) {
    message("The Slurm_log function only works in interactive mode.")
    return(invisible())
  }

  # Checking if the job has been submitted, and if the files are available.
  if (status(x) == -1)
    stop("It seems that the job hasn't started yet.", call. = FALSE)

  location <- paste0(x$opts_r$tmp_path, "/", x$opts_job$`job-name`)
  if (!dir.exists(location))
    stop(
      "While the job seems to have started, the path to its temp files ",
      "does not exists.", call. = FALSE
    )

  # Listing log files and checking if we can look at one of these.
  if (is.null(which.)) {

    logs <- list.files(location, pattern = "^02[-]output", full.names = TRUE)
    if (!length(logs))
      stop(
        "There are not log files in the `tmp_path` of the job. ",
        "Perhaps the job is still initializing.",
        call. = FALSE
      )

    logs <- logs[1]

  } else {

    # Is it within the expected range?
    if ((length(which.) > 1L) || !is.numeric(which.))
      stop("`which.` should be an integer of length 1.", call. = FALSE)

    if (which. > x$njobs || which. < 1L)
      stop("`which.` should be within 1 and x$njobs.", call. = FALSE)

    logs <- paste0(location, "/02-output-", x$jobid, "-", which., ".out")
    if (!file.exists(logs))
      stop("The requested logfile does not exists.", call. = FALSE)

  }

  # Checking if less is available on the system
  less_available <- tryCatch(
    system2("type", cmd, stderr = TRUE, stdout = TRUE),
    error = function(e) e)

  if (inherits(x, "error"))
    stop(
      cmd," is not available in your system. Talk to your system admin.",
      call. = FALSE
    )

  key <- ""
  while (!(key %in% c("y", "n"))) {
    key <- readline(paste0(
      "You are about to call \"", cmd, "\"  to checkout the log file of\n",
      "your Slurm job #", x$jobid, ". You can also checkout the log file in\n",
      "the following location:\n  ", logs,"\nContinue? Yes(y) / No(n). "
    ))

    if (!(key %in% c("y", "n", ""))) {
      cat("You must press either y or n.\n")
      next
    } else if (key == "n")
      return(invisible())

    break
  }

  system2(cmd, logs)

}
