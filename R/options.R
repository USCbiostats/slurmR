
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

  # Default job_path
  job_path <- getwd()

  # Function to set job path
  set_job_path <- function(path, recursive = TRUE) {

    if (!dir.exists(path)) {
      dir.create(path, recursive = recursive)
    }

    job_path <<- path

    invisible()
  }

  # Function to get the path
  get_job_path <- function() {
    job_path
  }

  list2env(
    list(
      set_job_path = set_job_path,
      get_job_path = get_job_path
    )
  )

})()

