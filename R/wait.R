#' Wait for a Slurm job to be completed
#' @param x Either a job id number, or an object of class [slurm_job].
#' @param freq Frequency in seconds to query for the state of the job.
#' @param force Logical scalar. When `TRUE`, if the job is not found
#' after checking for its status, the function will continue to wait
#' still.
#' @param timeout Integer. Maximum wait time in seconds. If `timeout < 0`
#' then the command will only return when the job finishes.
#' @param ... Further arguments passed to the method
#' @export
#' @return Invisible `NULL`.
#' @examples
#' # Waiting is only available if there are Slurm clusters
#' if (slurm_available()) {
#'   job <- Slurm_EvalQ(Sys.sleep(1000), plan = "submit", njobs = 2)
#'   wait_slurm(job, timeout = 1) # This will return a warning
#'   Slurm_clean(job)
#' }
wait_slurm <- function(x, ...) UseMethod("wait_slurm")

#' @export
#' @rdname wait_slurm
wait_slurm.slurm_job <- function(x, ...) {
  wait_slurm.integer(get_job_id(x), ...)
}

#' @export
#' @rdname wait_slurm
wait_slurm.integer <- function(x, timeout = -1, freq = 0.1, force = TRUE, ...) {

  # Checking if Slurm and debug mode
  if (opts_slurmR$get_debug()) {
    warning("waiting is not available in debug mode.", call. = FALSE)
    return()
  } else if (!slurm_available())
    stopifnot_slurm()

  time0 <- Sys.time()
  while(TRUE) {

    # Getting status, every freq secs
    Sys.sleep(freq)
    s <- status(x)

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

    if (timeout > 0) {
      seconds <- difftime(Sys.time(), time0, units = "secs")
      if (seconds > timeout) {
        warning("Timeout after ", seconds, " seconds.", call. = FALSE, immediate. = TRUE)
        return(invisible(NULL))
      }
    }

  }

  invisible(NULL)

}
