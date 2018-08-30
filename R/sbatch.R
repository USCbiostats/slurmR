#' A wrapper of `sbatch`.
#'
#' @param x An object of class `slurm_job`.
#' @param wait Logical scalar. When `TRUE` the function will pass the `--wait`
#' flag to `Slurm` and set `wait=TRUE` in the [system2] call.
#' @param ... Further flags passed to the command line function.
#' @export
sbatch <- function(x, wait=TRUE, ...) UseMethod("sbatch")

process_dots <- function(...) {
  dots   <- list(...)
  option <- ifelse(
    nchar(names(dots)) > 1,
    paste0("--", names(dots)),
    paste0("-", names(dots))
  )

  vals <- character(length(option))
  for (i in seq_along(dots)) {
    if (is.logical(dots[[i]]) && !dots[[i]])
      option[i] <- ""
    else if (!is.logical(dots[[i]]))
      vals[i] <- paste0("=", dots[[i]])
  }

  sprintf("%s%s", option, vals)
}

check_error <- function(cmd, ans) {
  if (inherits(ans, "error")) {
    stop("`",cmd,"` not found. It seems that your system does not have Slurm. ",
         call. = FALSE)
  } else if (length(attr(ans, "status")) && (attr(ans, "status") != 0)) {
    stop(
      "An error has occurred when calling `", cmd,"`:\n",
      paste(ans, collapse="\n"), call. = FALSE)
  }
}

#' @export
#' @rdname sbatch
sbatch.slurm_job <- function(x, wait=TRUE, ...) {

  # Change dir argument
  chdir <- sprintf("%s/%s", x$job_path(), x$job_name())

  if (!is.na(x$job_id))
    stop("Job ", x$job_id," is already running.")

  # Preparing options
  option <- c(process_dots(..., wait=wait, chdir=chdir), x$batchfile)

  message("Submitting job...")
  ans <- suppressWarnings({
    tryCatch(system2("sbatch", option, stdout=TRUE, stderr = TRUE, wait=wait),
                  error=function(e) e)
  })

  # Checking errors
  check_error("sbatch", ans)

  # Warning that the call has been made and storing the id
  x$job_id <- as.integer(gsub(pattern = ".+ (?=[0-9]+$)", "", ans, perl=TRUE))

  x

}

#' @export
#' @rdname sbatch
scancel <- function(x, ...) UseMethod("scancel")

#' @export
#' @rdname sbatch
scancel.slurm_job <- function(x, ...) {

  if (is.na(x$job_id)) {
    warning("This job hasn't started yet. Nothing to cancel.", call. = FALSE)
    return()
  }

  # Preparing options
  option <- c(process_dots(...), x$job_id)

  ans <- suppressWarnings({
    tryCatch(system2("scancel", option, stdout=TRUE, stderr = TRUE),
             error=function(e) e)
  })

  # Checking errors
  check_error("sbatch", ans)

  invisible()

}
