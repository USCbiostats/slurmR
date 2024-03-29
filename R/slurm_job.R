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

check_hooks <- function(x) {

  if (length(x) == 0)
    return(invisible())

  if (!inherits(x, "list"))
    stop(
      "The -hooks- parameter should be of class \"list\". It is of class:\n\"",
      paste(class(x), collapse = "\", \""),
      call. = FALSE
    )

  test <- !sapply(x, is.function)
  if (any(test))
    stop(
      "The hook(s): ", paste0(which(test), collapse = ", "), " are not ",
      "functions. All hooks (if any) should be functions.", call. = FALSE
    )

  return(invisible())

}

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
  check_hooks(hooks)

  job <- structure(list2env(
    list(
      call     = call,
      rscript  = rscript,
      bashfile = bashfile,
      robjects = NULL,
      njobs    = njobs,
      opts_job = opts_job,
      opts_r   = opts_r,
      jobid    = NA_integer_,
      hooks    = hooks
    ),
    envir = new.env(parent = emptyenv())
  ), class = "slurm_job"
  )

  # Storing job in folder
  saveRDS(
    job,
    file = snames(
      "job",
      tmp_path = opts_r$tmp_path,
      job_name = opts_job$`job-name`
    ),
    compress = FALSE
  )

  return(job)

}

# Job name ---------------------------------------------------------------------
get_job_name <- function(x) UseMethod("get_job_name")

get_job_name.slurm_job <- function(x) x$opts_job$`job-name`

get_job_name.integer <- function(x) {

  if (is.na(last_job()$jobid))
    stop(
      "Cannot get the path for job -", x, "-. You can only call this function ",
      "right after submitting a job, or when -x- is of class \'slurm_job\'. ",
      "-x- is of class ", class(x),
    )

  if (last_job()$jobid == x) {
    return(get_job_name(last_job()))
  }

  stop(
    "The jobid ", x, " cannot be matched to the last job submitted (id: ",
    last_job()$jobid, ".)"
  )

}

# Job path ---------------------------------------------------------------------
get_tmp_path <- function(x) UseMethod("get_tmp_path")

get_tmp_path.slurm_job <- function(x) x$opts_r$tmp_path

get_tmp_path.integer <- function(x) {

  if (is.na(last_job()$jobid))
    stop(
      "Cannot get the path for job -", x, "-. You can only call this function ",
      "right after submitting a job, or when -x- is of class \'slurm_job\'. ",
      "-x- is of class ", class(x),
      )

  if (last_job()$jobid == x) {
    return(get_tmp_path(last_job()))
  }

  stop(
    "The jobid ", x, " cannot be matched to the last job submitted (id: ",
       last_job()$jobid, ".)"
    )

}

# Getting the job id (Slurm) ---------------------------------------------------
get_job_id <- function(x) UseMethod("get_job_id")
get_job_id.slurm_job <- function(x) x$jobid

`get_job_id<-` <- function(x, value) UseMethod("get_job_id<-")

`get_job_id<-.slurm_job` <- function(x, value) {

  if (length(value) != 1L)
    stop("Incorrect length for job ID.", call. = FALSE)

  value <- as.integer(value)

  if (!opts_slurmR$get_debug() && !is.finite(value))
    stop("job IDs must be finite. This is the current value trying to assign: ",
	 value, ".", call. = FALSE)

  x$jobid <- value
  x
}

get_job_id.slurm_cluster <- function(x) attr(x, "SLURM_JOBID")

get_job_id.slurm_hosts <- function(x) x$jobid

get_job_id.integer <- function(x) {

  if (is.na(last_job()$jobid))
    stop(
      "Cannot get the path for job -", x, "-. You can only call this function ",
      "right after submitting a job, or when -x- is of class \'slurm_job\'. ",
      "-x- is of class ", class(x),
    )

  if (last_job()$jobid == x) {
    return(x)
  }

  stop(
    "The jobid ", x, " cannot be matched to the last job submitted (id: ",
    last_job()$jobid, ".)"
  )

}


# Personalized errors
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
    sprintf("njobs (size) : %i\n", x$njobs),
    sprintf("job_name     : %s\n", get_job_name(x)),
    sprintf("tmp_path     : %s\n", get_tmp_path(x)),
    "All auxiliray files are located at:\n",
    sprintf("\t%s/%s\n", get_tmp_path(x), get_job_name(x)),
    sprintf("job ID       : %s\n",
            ifelse(
              is.na(get_job_id(x)),
              "Not submitted",
              as.character(get_job_id(x))
            )
    ), sep=""
  )

  if (!is.na(get_job_id(x))) {
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

    path <- normalizePath(file.path(path, "job.rds"))
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
#' @return In the case of the function `write_slurm_job`, it returns the full
#' path to the file.
write_slurm_job <- function(
  x,
  path = NULL
  ) {

  stopifnot_slurm_job(x)

  # Setting the old ones
  if (is.null(path)) {
    path <- snames(
      "job",
      tmp_path = get_tmp_path(x),
      job_name = get_job_name(x)
      )
  }

  saveRDS(x, path, compress = FALSE)

  invisible(path)

}



#' This environment sets and gets the latest submitted job. The function
#' [last_submitted_job] is a wrapper of it visible to the user.
#' @noRd
LAST_SUBMITTED_JOB <- (function() {

  record <- new.env(parent = emptyenv())

  record$job <- NULL
  record$set <- function(job) {

    if (!inherits(job, "slurm_job"))
      stop("The `job` argument must be an object of class `slurm_job`.",
           call. = FALSE)

    record$job <- job

    invisible()
  }
  record$get <- function() {
    record$job
  }

  return(record)

})()

#' @rdname slurm_job
#' @export
#' @details The `las_submitted_job` function will return the latest `slurm_job`
#' object that was submitted via [sbatch] in the current session. The `last_job`
#' function is just an alias of the later. If no job has been submitted, then
#' the resulting value will be `NULL`.
#' @examples
#' \dontrun{
#' # The last_job function can be handy when `plan = "collect"` in a called,
#' # for example
#' job <- Slurm_lapply(1:1000, function(i) runif(100), njobs = 2, plan = "collect")
#'
#' # Post collection analysis
#' status(last_job())
#' }
last_submitted_job <- function() {

  LAST_SUBMITTED_JOB$get()

}


#' @export
#' @rdname slurm_job
last_job <- last_submitted_job
