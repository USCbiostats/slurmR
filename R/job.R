#' Slurm Jobs
#' @param call The original call
#' @param rscript,bashfile The R script and bash file path.
#' @param robjects A character vector of R objects that will be imported in the job.
#' @param job_name,job_path Character. Name and path of the job.
#' @param njobs Integer. Number of jobs to start (array).
#' @export
new_slurm_job <- function(
  call,
  rscript,
  bashfile,
  robjects,
  job_name,
  job_path,
  njobs
  ) {

  job <- list2env(
    list(
      call     = call,
      rscript  = rscript,
      bashfile = bashfile,
      robjects = robjects,
      job_name = job_name,
      job_path = job_path,
      njobs    = njobs,
      job_id   = NA
    ),
    envir = new.env(parent = emptyenv())
  )

  structure(
    job,
    class = "slurm_job"
  )

}



#' A wrapper of `sbatch`.
#'
#' @param x An object of class `slurm_job`.
#' @param wait Logical scalar. When `TRUE` the function will pass the `--wait`
#' flag to `Slurm` and set `wait=TRUE` in the [system2] call.
#' @param ... Further flags passed to the command line function.
#' @export
sbatch <- function(x, wait=TRUE, ...) UseMethod("sbatch")

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

  # Checking the status of the job
  if (!is.na(x$job_id) && squeue(x$job_id))
      stop("Job ", x$job_id," is already running.")

  # Preparing options
  option <- c(
    sprintf("%s/%s && sbatch", x$job_path, x$job_name),
    parse_flags(...),
    x$bashfile
    )

  message("Submitting job...", appendLF = FALSE)
  ans <- suppressWarnings({
    tryCatch(system2("cd", option, stdout = TRUE, wait=TRUE),
                  error=function(e) e)
  })

  # Checking errors
  check_error("sbatch", ans)

  # Warning that the call has been made and storing the id
  x$job_id <- as.integer(gsub(pattern = ".+ (?=[0-9]+$)", "", ans, perl=TRUE))
  message(" jobid:", x$job_id, ".")

  if (wait) {
    message("Waiting for the job to be done...", appendLF = FALSE)
    ans <- sbatch_dummy(
      `job-name` = paste0(x$job_name, "-dummy"),
      dependency=paste0("afterany:", x$job_id)
      )
    check_error("srun", ans)
    message("Done.")
  }

  # Not necesary
  invisible(x)

}

#' Waits for the `job_id` to be completed.
#' @noRd
sbatch_dummy <- function(...) {

  flags <- parse_flags(
    c(
      list(
        output = "/dev/null",
        quiet  = TRUE,
        wait   = TRUE
        ),
      ...)
    )

  # Dummy file to run sbatch
  tmp <- tempfile(fileext = ".sbatch")
  writeLines("#!/bin/sh\n\necho 0", tmp)

  cmd <- sprintf("%s %s", paste(flags, collapse=" "), tmp)
  ans <- system2("sbatch", cmd, wait = TRUE, stdout=TRUE)

  structure(ans, cmd = paste("srun", cmd))


}


#' @export
#' @rdname sbatch
squeue <- function(x, ...) UseMethod("squeue")

#' @export
#' @rdname sbatch
squeue.slurm_job <- function(x, ...) {

  # Preparing options
  option <- c(
    sprintf("-j%i", x$job_id),
    parse_flags(...)
  )

  # message("Submitting job...")
  ans <- suppressWarnings({
    tryCatch(system2("squeue", option, stdout=TRUE, stderr = TRUE, wait=TRUE),
             error=function(e) e)
  })

  ans

}

#' @export
print.slurm_job <- function(x, ...) {

  cat("Call:\n", paste(deparse(x$call), collapse="\n"), "\n")
  cat(
    sprintf("job_name : %s\n", x$job_name),
    sprintf("path     : %s/%s\n", x$job_path, x$job_name),
    sprintf("job ID   : %s\n",
            ifelse(
              is.na(x$job_id),
              "Not submitted",
              as.character(x$job_id)
            )
    ), sep=""
  )

  if (!is.na(x$job_id))
    # cat(squeue(x), sep="\n")

  invisible(x)
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
  option <- c(parse_flags(...), x$job_id)

  ans <- suppressWarnings({
    tryCatch(system2("scancel", option, stdout=TRUE, stderr = TRUE),
             error=function(e) e)
  })

  # Checking errors
  check_error("sbatch", ans)

  x$job_id <- NA

  invisible()

}

#' @rdname sbatch
#' @export
sacct <- function(x) UseMethod("sacct")

#' @export
#' @rdname sbatch
sacct.slurm_job <- function(x) {

  sacct.default(x$job_id)

}

#' @export
#' @rdname sbatch
sacct.default <- function(x, ...) {

  ans <- suppressWarnings({
    system2("sacct", paste0("--brief --parsable --jobs=", x), stdout = TRUE)
  })

  check_error("sbatch", ans)

  ans <- lapply(ans, strsplit, split="|", fixed=TRUE)
  ans <- do.call(rbind, lapply(ans, unlist))

  structure(
    as.data.frame(ans[-1,], stringsAsFactors=FALSE),
    names = ans[1,]
  )


}


Slurm_status <- function(x) {

  dat <- sacct(x)

  if (!nrow(dat)) {
    return(1)
  } else if (all(dat$State == "COMPLETED"))
    return(0)
  else return(2)

}

