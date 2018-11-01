
#' Get and set default options for `sbatch` and `sluRm` internals
#'
#' Most of the functions in the `sluRm` package use `chdir` and `job-name`
#' options to write and submit jobs to **Slurm**. These options have global
#' defaults that are set and retrieved using `opts_sluRm`.
#'
#' @details Current supported options are:
#'
#' - `debug_off : function ()` Deactivates the debug mode.
#'
#' - `debug_on : function ()` Activates the debug mode.
#'
#' - `get_chdir : function ()` Gets `chdir`
#'
#' - `get_cmd : function ()` Gets the bash command (see the *Debug code* section)
#'
#' - `get_debug : function ()` Returns `TRUE` if the debug mode is on. `FALSE`
#'   otherwise.
#'
#' - `get_job_name : function (check = TRUE)` Returns the current value of `job-name`
#'
#' - `set_chdir : function (path, recursive = TRUE)` Changes `chdir`.
#'
#' - `set_job_name : function (path, check = TRUE, overwrite = TRUE)` Changes
#'   the `job-name`. When changing the name of the job the function will check
#'   whether the folder `chdir/job-name` is empty or not. If empty/not created
#'   it will create it, otherwise it will delete its contents (if `overwrite = TRUE`,
#'   else it will return with an Error).
#'
#'
#' For general set/retrieve options
#'
#' - `get_opts : function (...)`
#'
#' - `set_opts : function (...)`
#'
#'
#' @examples
#'
#' # Common setup
#' \dontrun{
#' opts_sluRm$set_chdir("/staging/pdt/vegayon")
#' opts_sluRm$set_job_name("simulations-1")
#' opts_slurm$set_opts(partition="thomas", account="lc_pdt")
#' }
#'
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
  set_chdir <- function(path, recursive = TRUE, overwrite = FALSE) {

    # Path normalization
    path <- normalizePath(path)

    if (!length(path))
      return(get_chdir())

    # if (!dir.exists(path)) {
    #   dir.create(path, recursive = recursive)
    # }

    OPTS_SLURM$chdir <- path

    # Move location of the files
    if (length(OPTS_SLURM$`job-name`) && (get_chdir() != path))
      set_job_name(get_job_name(), overwrite = overwrite)

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

    if (!length(path))
      stop("`path` cannot be NULL", call.=FALSE)
    else if (path == "")
      stop("`path` must be a meaningful name. Cannot be \"\" (empty).", call.=FALSE)

    fn <- sprintf("%s/%s", get_chdir(), path)

    if (overwrite && file.exists(fn)) {
      warning("The path '", fn, "' already exists and will be overwritten.",
              call. = FALSE)
      status <- file.remove(list.files(fn, full.names = TRUE))
    }
    # else if (!file.exists(fn))
    #   dir.create(fn)

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
    test <- names(dots)[which(names(dots) %in% c('job-name', "chdir"))]
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
            " opts_sluRm$debug_off().\n\n")
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

.onLoad <- function(libname, pkgname) {
  opts_sluRm$set_chdir(getwd())

  tmp <- tempfile("sluRm-job-", opts_sluRm$get_chdir())
  tmp <- gsub(".+(?=sluRm-job-)", "", tmp, perl = TRUE)

  opts_sluRm$set_job_name(tmp)
}

