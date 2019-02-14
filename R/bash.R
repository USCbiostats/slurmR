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

silent_system2 <- function(...) {

  fun_name <- as.character(sys.call()[[1]])

  ans <- suppressWarnings({
    tryCatch(system2(...), error = function(e) e)
  })

  check_error(fun_name, ans)
  ans

}

#' Slurm Jobs
#' @param call The original call
#' @param rscript,bashfile The R script and bash file path.
#' @param robjects A character vector of R objects that will be imported in the job.
#' @param job_opts List. Arguments to be passed to [sbatch].
#' @param njobs Integer. Number of jobs to start (array).
#' @export
new_slurm_job <- function(
  call,
  rscript,
  bashfile,
  robjects,
  njobs,
  job_opts
  ) {

  job <- list2env(
    list(
      call     = call,
      rscript  = rscript,
      bashfile = bashfile,
      robjects = robjects,
      njobs    = njobs,
      job_opts = job_opts,
      jobid    = NA
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
#' @param submit Logical, when `TRUE` calls [sbatch] to submit the job to slurm.
#' @param ... Further flags passed to the command line function.
#' @export
sbatch <- function(x, wait=TRUE, submit = TRUE, ...) UseMethod("sbatch")

hline <- function(..., sep="\n") {
  cat("\n",rep("-", options("width")), "\n",sep="")
  do.call(cat, c(list(...), sep=sep))
  cat(rep("-", options("width")), "\n",sep="")
}

#' @export
#' @rdname sbatch
sbatch.slurm_job <- function(x, wait=TRUE, submit = TRUE, ...) {

  # Checking the status of the job
  if (!is.na(x$jobid) && squeue(x$jobid))
      stop("Job ", x$jobid," is already running.")

  # Preparing options
  option <- x$bashfile

  if (!opts_sluRm$get_debug()) {
    option <- c(parse_flags(c(x$job_opts,...)), option)
  } else {
    option <- c(option, paste(">", snames("out"), ifelse(wait, "", "&")))
  }

  if (opts_sluRm$get_verbose()) {
    hline(
      "`opts_sluRm$get_verbose() == TRUE`. The R script that will be used is located at:",
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
    message("Submitting job...", appendLF = FALSE)
    ans <- silent_system2(opts_sluRm$get_cmd(), option, stdout = TRUE, wait=TRUE)
  } else {
    warning(
      "`submit = FALSE`, which means that the job hasn't been submitted yet.",
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
  } else
    x$jobid <- NA

  if (wait) {

    ans <- sbatch_dummy(
      sh_cmd     = x$sh_cmd,
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

#' Waits for the `jobid` to be completed.
#' @noRd
sbatch_dummy <- function(...) {

  if (opts_sluRm$get_debug()) {
    warning("Waiting is not available in debug mode.")
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
squeue <- function(x, ...) UseMethod("squeue")

#' @export
#' @rdname sbatch
squeue.slurm_job <- function(x, ...) {

  # Preparing options
  option <- c(
    sprintf("-j%i", x$jobid),
    parse_flags(...)
  )

  # message("Submitting job...")
  ans <- silent_system2("squeue", option, stdout=TRUE, stderr = TRUE, wait=TRUE)

  ans

}

#' @export
print.slurm_job <- function(x, ...) {

  cat("Call:\n", paste(deparse(x$call), collapse="\n"), "\n")
  cat(
    sprintf("job_name : %s\n", x$job_name),
    sprintf("path     : %s/%s\n", x$job_opts$chdir, x$job_opts$`job-name`),
    sprintf("job ID   : %s\n",
            ifelse(
              is.na(x$jobid),
              "Not submitted",
              as.character(x$jobid)
            )
    ), sep=""
  )

  if (!is.na(x$jobid))
    print(sacct(x))

  invisible(x)
}



#' @export
#' @rdname sbatch
scancel <- function(x, ...) UseMethod("scancel")

#' @export
#' @rdname sbatch
scancel.slurm_job <- function(x, ...) {

  if (is.na(x$jobid)) {
    warning("This job hasn't started yet. Nothing to cancel.", call. = FALSE)
    return()
  }

  # Preparing options
  option <- c(parse_flags(...), x$jobid)

  ans <- silent_system2("scancel", option, stdout=TRUE, stderr = TRUE)

  x$jobid <- NA

  invisible()

}

#' @rdname sbatch
#' @export
sacct <- function(x, ...) UseMethod("sacct")

#' @export
#' @rdname sbatch
sacct.slurm_job <- function(x, ...) {

  sacct.default(x$jobid, ...)

}

#' @export
#' @param brief Logical. Passed to `sacct`.
#' @param parsable Logical. Passed to `sacct`.
#' @rdname sbatch
sacct.default <- function(x, brief=TRUE, parsable = TRUE, ...) {

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


