
#' Get and set default options for slurm
#' @details Current supported options are:
#' -  `chdir`: This can be retrieved as
#'
#'    ```
#'    opts_sluRm$get_chdir()
#'    ```
#'
#'    Likewise it can be set as
#'
#'    ```
#'    opts_sluRm$get_chdir("some_path")
#'    ```
#'
#'    Currently, the default value is set when loading the package and is the
#'    current working directory.
#'
#' -  ``
#' @export
opts_sluRm <- (function() {

  # Default chdir is null and will be set at the first call of the function
  OPTS_SLURM            <- new.env(parent = emptyenv())
  OPTS_SLURM$chdir      <- NULL
  OPTS_SLURM$`job-name` <- NULL

  OPTS_R       <- new.env(parent = emptyenv())
  OPTS_R$debug <- FALSE
  OPTS_R$cmd   <- "sbatch"


  # JOB PATH -------------------------------------------------------------------
  # Function to set job path
  set_chdir <- function(path, recursive = TRUE) {

    if (!length(path))
      return(get_chdir())

    if (!dir.exists(path)) {
      dir.create(path, recursive = recursive)
    }

    OPTS_SLURM$chdir <- path

    invisible()
  }

  # Function to get the path
  get_chdir <- function() {
    if (!length(OPTS_SLURM$chdir))
      OPTS_SLURM$chdir <- getwd()
    OPTS_SLURM$chdir
  }

  # JOB NAME -------------------------------------------------------------------
  set_job_name <- function(path, check = TRUE, overwrite = TRUE) {

    if (check && !length(path))
      stop("`path` cannot be NULL.", call. = FALSE)
    else if (!check && !length(path))
      return()

    fn <- sprintf("%s/%s", get_chdir(), path)
    if (overwrite && file.exists(fn)) {
      warning("The path '", fn, "' already exists and will be overwritten.",
              call. = FALSE)
      status <- file.remove(list.files(fn, full.names = TRUE))
    }
    else if (!file.exists(fn))
      dir.create(fn)

    OPTS_SLURM$`job-name` <- path

    invisible()

  }

  get_job_name <- function(check=TRUE) {
    if (check && !length(OPTS_SLURM$`job-name`))
      stop("Slurm job has not been initialized", call. = FALSE)
    OPTS_SLURM$`job-name`
  }

  # Generalized set/get function -----------------------------------------------
  set_opts <- function(...) {

    dots <- list(...)

    # Checking if one or more have already set
    test <- names(dots)[which(names(dots) %in% ls(envir = OPTS_SLURM))]
    if (length(test))
      warning("The following options can be set via `opts_sluRm$set_*`: `",
              paste0(test, collapse="`, `"), "`.")

    Map(
      function(x., value.) assign(x=x., value=value., envir = OPTS_SLURM),
      x. = names(dots), value. = unname(dots)
      )

    invisible()

  }

  get_opts <- function(...) {

    dots <- list(...)
    if (!length(dots))
      return(as.list(OPTS_SLURM))

    if (any(is.character(dots)))
      stop("`...` only receives characters.", call. = FALSE)

    as.list(OPTS_SLURM)[dots]

  }

  debug_on <- function() {
    OPTS_R$debug <- TRUE
    OPTS_R$cmd   <- "sh"
    message("Debug mode is now active. Which means that jobs will be called via",
            " `sh` and not `sbatch`. You can de-activate debug mode by calling",
            " opts_sluRm$debug_off().")
    invisible()
  }

  debug_off <- function() {
    OPTS_R$debug <- FALSE
    OPTS_R$cmd   <- "sbatch"
    invisible()
  }

  structure(list2env(
    list(
      set_chdir    = set_chdir,
      get_chdir    = get_chdir,
      set_job_name = set_job_name,
      get_job_name = get_job_name,
      set_opts     = set_opts,
      get_opts     = get_opts,
      debug_on     = debug_on,
      debug_off    = debug_off,
      get_debug    = function() OPTS_R$debug,
      get_cmd      = function() OPTS_R$cmd
    )
  ), class = "opts_sluRm")

})()

#' @export
print.opts_sluRm <- function(x, ...) {

  cat("Current default for Slurm jobs:\n\n")
  str(x$get_opts())
  cat("\nTo get and set options for Slurm jobs creation use (see ?opts_sluRm):\n\n")
  print(utils::ls.str(x))

  if(x$get_debug())
    cat("Debugging mode is currently on, which means that `sbatch` will use `sh`",
        " instead (to deactivate it use opts_sluRm$debug_off().\n")

  invisible(x)

}


