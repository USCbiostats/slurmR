#' @details The function `slurm_available` checks whether Slurm is available in
#' the system or not. It is usually called before calling any bash wrapper.
#' If available, the function will return `TRUE`, otherwise `FALSE`.
#'
#' @examples
#' # Are we under a Slurm Cluster?
#' slurm_available()
#'
#' @export
#' @rdname sbatch
slurm_available <- function() {

  if (.Platform$OS.type != "unix")
    return(FALSE)

  # It is available if R can find it!
  x <- Sys.which("sbatch")
  return(nchar(x) > 0L)

}

#' @export
#' @rdname sbatch
#' @details The wrapper of squeue includes the flag `-o%all` which returns all
#' available fields separated by a vertical bar. This cannot be changed since it
#' is the easiest way of processing the information in R.
squeue <- function(x = NULL, ...) UseMethod("squeue")

#' @export
#' @rdname sbatch
squeue.default <- function(x = NULL, ...) {

  # Checking for slurm
  stopifnot_slurm()

  # Notice that the jobid may be null
  option <- c(sprintf("-j%i", x), "-o%all", parse_flags(...))

  # message("Submitting job...")
  ans <- silent_system2("squeue", option, stdout = TRUE, stderr = TRUE)

  # Parsing the data
  ans <- lapply(ans, strsplit, split="|", fixed=TRUE)
  ans <- do.call(rbind, lapply(ans, unlist))

  structure(
    as.data.frame(ans[-1, , drop=FALSE], stringsAsFactors=FALSE),
    names = ans[1,]
  )

}

#' @export
#' @rdname sbatch
squeue.slurm_job <- function(x, ...) {

  stopifnot_submitted(get_job_id(x))
  squeue.default(get_job_id(x), ...)

}

#' @export
#' @rdname sbatch
scancel <- function(x = NULL, ...) UseMethod("scancel")

#' @export
#' @rdname sbatch
scancel.default <- function(x = NULL, ...) {

  # Checking for slurm, and if passes, if it started
  stopifnot_slurm()

  # Preparing options
  option <- c(parse_flags(...), x)

  ans <- silent_system2("scancel", option, stdout = TRUE, stderr = TRUE)
  wait_slurm(x)

  invisible()

}

#' @export
#' @rdname sbatch
scancel.slurm_job <- function(x = NULL, ...) {

  stopifnot_submitted(get_job_id(x))
  scancel.default(get_job_id(x), ...)

}

#' @rdname sbatch
#' @export
sacct <- function(x, ...) UseMethod("sacct")

#' @export
#' @param brief,parsable,allocations Logical. When `TRUE`, these are passed as flags directly
#' to the command line function `sacct`.
#' @rdname sbatch
sacct.default <- function(
  x           = NULL,
  brief       = TRUE,
  parsable    = TRUE,
  allocations = TRUE,
  ...
  ) {

  # Checking for slurm
  stopifnot_slurm()

  flags <- parse_flags(
    c(
      list(brief = brief, parsable = parsable, jobs = x, allocations = allocations),
      list(...)
      )
    )

  ans <- silent_system2("sacct", flags, stdout = TRUE, stderr = TRUE)

  ans <- lapply(ans, strsplit, split="|", fixed=TRUE)
  ans <- do.call(rbind, lapply(ans, unlist))

  structure(
    as.data.frame(ans[-1, , drop=FALSE], stringsAsFactors=FALSE),
    names = ans[1,]
  )

}

#' @export
#' @rdname sbatch
sacct.slurm_job <- function(x, ...) {

  stopifnot_submitted(get_job_id(x))
  sacct.default(get_job_id(x), ...)

}


#' @export
#' @rdname sbatch
#' @details The function `slurm.conf` is a wrapper of the function `scontrol` that
#' returns configuration info about Slurm, in particular, the underlying command
#' that is called is `scontrol show conf`. This returns a named character vector
#' with configuration info about the cluster. The name of this function matches
#' the name of the file that holds this information.
#' @examples
#'
#' \dontrun{
#' # What is the maximum number of jobs (array size) that the system
#' # allows?
#' sconfig <- slurm.conf() # We first retrieve the info.
#' sconfig["MaxArraySize"]
#' }
#'
slurm.conf <- function() {

  stopifnot_slurm()
  conf <- silent_system2("scontrol", "show conf", stdout = TRUE, stderr = TRUE)

  conf <- conf[grepl("^[[:graph:]]+\\s*[=]", conf)]

  structure(
    gsub("^[[:graph:]]+\\s*[=]\\s*", "", conf),
    names = gsub("\\s*[=].+", "", conf)
  )

}

#' @export
#' @rdname sbatch
#' @details The function `SchedulerParameters` is just a wrapper of [slurm.conf].
#' It processes the field "SchedulerParameters" included in the configuration
#' file and has information relevant for the scheduler.
SchedulerParameters <- function() {

  conf <- slurm.conf()["SchedulerParameters"]
  conf <- strsplit(x = conf, split = ",", fixed = TRUE)[[1]]

  # Which are logicals
  logicals <- grepl("^[^=]+$", conf)

  values <- gsub("^.+[=]", "", conf)
  values[logicals] <- TRUE
  names(values) <- gsub("[=].+", "", conf)

  values <- as.list(values)
  values[] <- lapply(values, function(v) {
    if (all(grepl("^[0-9]+$", v)))
      as.integer(v)
    else if (all(v == "TRUE")) {
      rep(TRUE, length(v))
    } else
      v
      })

  return(as.list(values))
}
