#' Creating Slurm jobs
#'
#' Utilities to deal with objects of class `slurm_job`. The function `new_slurm_job`,
#' which is mostly intended to be for internal used, creates an object of class
#' `slurm_job`. The function `last_submitted_job` returns the last submitted
#' job in the current R session, and the functions `read/write_slurm_job` are
#' utility functions to read and write R jobs respectively.
#'
#' @param call The original call
#' @param rscript,bashfile The R script and bash file path.
#' @param robjects A character vector of R objects that will be imported in the job.
#' @param opts_job,opts_r List. In the case of `opts_job`, a list of parameters
#' passed to [sbatch]. `opts_r` is a list of parameters used within R. Both can
#' be retrieved by [opts_slurmR]`$get_opts_job()` and [opts_slurmR]`$get_opts_r()`
#' respectively.
#' @param njobs Integer. Number of jobs to start (array).
#' @param hooks List of functions. To be called on the collected results after
#' it finalizes.
#' @param x An object of class `slurm_job`.
#' @param ... Further arguments passed to the method.
#' @name slurm_job
NULL

#' @export
#' @details In the case of the function `new_slurm_job`, besides of creating the
#' object of class `slurm_job`, the function calls `write_slurm_job` and stores
#' the job object in an [`rds`][saveRDS] class file. The name and location of
#' the saved rds file is generated using the function `snames("job")`.
#'
#' @rdname slurm_job
#' @return An environment of class `slurm_job`. This has the following items:
#' - `call` The original call ([Slurm_lapply], [Slurm_Map], etc.)
#' - `rscript` The full path to the R script to be executed by bash file.
#' - `bashfile` The full path to the bash file to be executed by [sbatch].
#' - `robjects` Ignored.
#' - `njobs` The number of jobs to be submitted (job array).
#' - `opts_job`,`opts_r` Two lists of options as returned by [opts_slurmR]$get_opts_job()
#' and [opts_slurmR]$get_r_opts() at the moment of the creation of the `slurm_job`.
#' - `hooks` A list of functions to be called on the collected objects
#' by [Slurm_collect].
#'
new_slurm_job <- function(
  call,
  rscript,
  bashfile,
  robjects,
  njobs,
  opts_job,
  opts_r,
  hooks = NULL
) {

  # Checking hooks
  if (!is.null(hooks)) {
    test <- !sapply(hooks, is.function)
    if (any(test))
      stop("The hook(s): ", paste0(which(test), collapse = ", "), " are not ",
           "functions. All hooks (if any) should be functions.", call. = FALSE
      )
  }

  job <- structure(list2env(
    list(
      call     = call,
      rscript  = rscript,
      bashfile = bashfile,
      robjects = NULL,
      njobs    = njobs,
      opts_job = opts_job,
      opts_r   = opts_r,
      jobid    = NA,
      hooks    = hooks
    ),
    envir = new.env(parent = emptyenv())
  ), class = "slurm_job"
  )

  # Storing job in folder
  saveRDS(job, file = snames("job"), compress = FALSE)

  return(job)

}

stopifnot_slurm_job <- function(x) {
  if (!inherits(x, "slurm_job"))
    stop("The passed object is not of class `slurm_job`.", call. = FALSE)

  invisible()
}

#' @export
#' @rdname slurm_job
print.slurm_job <- function(x, ...) {

  cat("Call:\n", paste(deparse(x$call), collapse="\n"), "\n")
  cat(
    sprintf("job_name : %s\n", x$job_name),
    sprintf("tmp_path : %s/%s\n", x$opts_r$tmp_path, x$opts_job$`job-name`),
    sprintf("job ID   : %s\n",
            ifelse(
              is.na(x$jobid),
              "Not submitted",
              as.character(x$jobid)
            )
    ), sep=""
  )

  if (!is.na(x$jobid)) {
    print(status(x))
  }

  invisible(x)
}


#' @rdname slurm_job
#' @param path Character scalar. Path to either a directory with a `job.rds` file,
#' or directly to a `job.rds` file.
#' @export
#' @details The `read_slurm_job` can help the user recovering a previously saved
#' `slurm_job` object. If `path` is a directory, then the function will assume
#' that the file that is looking for lives within that directory and is named
#' `job.rds`. Otherwise, if a file, then it will read it directly. In any case,
#' it will check that the read object is an object of class `slurm_job`.
read_slurm_job <- function(path) {

  if (dir.exists(path)) {

    path <- sprintf("%s/job.rds", normalizePath(path))
    if (!file.exists(path))
      stop(
        "The file `job.rds` does not exists in the specified `path`. ",
        "Are you sure that folder has a slurm_job object?", call. = FALSE
        )

    job <- readRDS(path)

  } else if (file.exists(path)) {

    job <- readRDS(path)

  } else
    stop(
      "Nor a file neither a directory as specified by `path` exists.",
      call. = FALSE
      )

  # After we read the object, we want to make sure that it actually is a
  # slurm_job object.
  if (!inherits(job, "slurm_job"))
    stop("The read object is not of class `slurm_job`.", call. = FALSE)

  return(job)

}

#' @rdname slurm_job
#' @export
#' @details The `write_slurm_job` function simply takes a `slurm_job` object
#' and saves it in, if `path` is not specified, whatever the `job$options$chdir`
#' folder is under the name `job.rds`. If a path is specified, the it is directly
#' passed to [saveRDS()].
write_slurm_job <- function(x, path = NULL) {

  stopifnot_slurm_job(x)

  # Setting the old ones
  if (is.null(path)) {
    oldopts <- c(
      opts_slurmR$get_opts_job("chdir"),
      opts_slurmR$get_opts_r("tmp_path")
    )
    on.exit({
      opts_slurmR$set_tmp_path(oldopts$tmp_path)
      opts_slurmR$set_job_name(oldopts$`job-name`)
      })

    opts_slurmR$set_tmp_path(x$opts_r$tmp_path)
    opts_slurmR$set_job_name(x$opts_job$`job-name`)

    path <- snames("job")
  }

  saveRDS(x, path, compress = FALSE)

}
