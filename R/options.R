
#' Get and set default options for slurm
#' @details Current supported options are:
#' -  `job_path`: This can be retrieved as
#'
#'    ```
#'    options_sluRm$get_job_path()
#'    ```
#'
#'    Likewise it can be set as
#'
#'    ```
#'    options_sluRm$get_job_path("some_path")
#'    ```
#'
#'    Currently, the default value is set when loading the package and is the
#'    current working directory.
#'
#' -  ``
#' @export
options_sluRm <- (function() {

  # Default job_path is null and will be set at the first call of the function
  job_path <- NULL
  job_name <- NULL

  # Function to set job path
  set_job_path <- function(path, recursive = TRUE) {

    if (!length(path))
      return(get_job_path())

    if (!dir.exists(path)) {
      dir.create(path, recursive = recursive)
    }

    job_path <<- path

    invisible()
  }

  # Function to get the path
  get_job_path <- function() {
    if (!length(job_path))
      job_path <<- getwd()
    job_path
  }

  set_job_name <- function(path, check = TRUE, overwrite = TRUE) {

    if (check && !length(path))
      stop("`path` cannot be NULL.", call. = FALSE)
    else if (!check && !length(path))
      return()

    fn <- sprintf("%s/%s", get_job_path(), path)
    if (overwrite && file.exists(fn)) {
      warning("The path '", fn, "' already exists and will be overwritten.",
              call. = FALSE)
      status <- file.remove(list.files(fn, full.names = TRUE))
    }
    else if (!file.exists(fn))
      dir.create(fn)

    job_name <<- path

    invisible()

  }

  get_job_name <- function(check=TRUE) {
    if (check && !length(job_name))
      stop("Slurm job has not been initialized", call. = FALSE)
    job_name
  }

  list2env(
    list(
      set_job_path = set_job_path,
      get_job_path = get_job_path,
      set_job_name = set_job_name,
      get_job_name = get_job_name
    )
  )

})()



