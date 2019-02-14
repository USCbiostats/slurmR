
#' Get and set default options for `sbatch` and `sluRm` internals
#'
#' Most of the functions in the `sluRm` package use `chdir` and `job-name`
#' options to write and submit jobs to **Slurm**. These options have global
#' defaults that are set and retrieved using `opts_sluRm`.
#'
#' @details Current supported options are:
#'
#' Debuggin mode
#'
#' - `debug_on : function ()` \Sexpr{attr(sluRm::opts_sluRm$debug_on, "desc")}
#'
#' - `debug_off : function ()` \Sexpr{attr(sluRm::opts_sluRm$debug_off, "desc")}
#'
#' - `get_debug : function ()` \Sexpr{attr(sluRm::opts_sluRm$get_debug, "desc")}
#'
#' Verbose mode
#'
#' - `verbose_on : function ()` \Sexpr{attr(sluRm::opts_sluRm$verbose_on, "desc")}
#'
#' - `verbose_off : function ()` \Sexpr{attr(sluRm::opts_sluRm$verbose_off, "desc")}
#'
#' - `get_verbose : function ()` \Sexpr{attr(sluRm::opts_sluRm$get_verbose, "desc")}
#'
#' Slurm options
#'
#' - `set_chdir : function (path, recursive = TRUE)` \Sexpr{attr(sluRm::opts_sluRm$set_chdir, "desc")}
#'
#' - `get_chdir : function ()` \Sexpr{attr(sluRm::opts_sluRm$get_chdir, "desc")}
#'
#' - `set_job_name : function (path, check = TRUE, overwrite = TRUE)` \Sexpr{attr(sluRm::opts_sluRm$set_job_name, "desc")}.
#'
#' - `get_job_name : function (check = TRUE)` \Sexpr{attr(sluRm::opts_sluRm$get_job_name, "desc")}
#'
#' Other options
#'
#' - `get_cmd : function ()` \Sexpr{attr(sluRm::opts_sluRm$get_cmd, "desc")}
#'
#'
#' For general set/retrieve options
#'
#' - `set_opts : function (...)` \Sexpr{attr(sluRm::opts_sluRm$set_opts, "desc")}
#'
#' - `get_opts : function (...)` \Sexpr{attr(sluRm::opts_sluRm$get_opts, "desc")}
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

  OPTS_R         <- new.env(parent = emptyenv())
  OPTS_R$debug   <- FALSE
  OPTS_R$cmd     <- "sbatch"
  OPTS_R$verbose <- FALSE


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

  attr(set_chdir, "desc") <- "Sets the working directory"
  attr(set_chdir, "desc") <- "Retrieves the working directory"

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

  attr(set_job_name, "desc") <- paste(
    "Changes the job-name. When changing the name of the job the function will",
    "check whether the folder chdir/job-name is empty or not. If empty/not",
    "created it will create it, otherwise it will delete its contents (if",
    "`overwrite = TRUE``, else it will return with an Error).")

  attr(get_job_name, "desc") <- "Returns the current value of `job-name`."

  # Generalized set/get function -----------------------------------------------
  set_opts <- function(...) {

    dots <- list(...)

    # Checking if one or more have already set
    test <- names(dots)[which(names(dots) %in% c('job-name', "chdir"))]
    if (length(test))
      warning("The following options can be set via `opts_sluRm$set_*`: `",
              paste0(test, collapse="`, `"), "`.")

    Map(
      function(x., value.) {
         if (!length(value.))
            rm(list = x., envir = OPTS_SLURM)
         else
            assign(x=x., value=value., envir = OPTS_SLURM)
      },
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

  attr(set_opts, "desc") <- "A generic function to set options."
  attr(get_opts, "desc") <- "A generic function to retrieve options."

  # Debugging and Verbose ------------------------------------------------------

  debug_on <- function() {
    OPTS_R$debug   <- TRUE
    OPTS_R$cmd     <- "sh"
    OPTS_R$verbose <- TRUE
    message("Debug mode is now active. Which means that jobs will be called via",
            " `sh` and not `sbatch`. You can de-activate debug mode by calling",
            " opts_sluRm$debug_off(). Notice that only 1/njobs will be submitted",
            ", so not all the data will be processed.")
    invisible()
  }

  debug_off <- function() {
    OPTS_R$debug   <- FALSE
    OPTS_R$cmd     <- "sbatch"
    OPTS_R$verbose <- FALSE
    invisible()
  }

  verbose_on <- function() {
    OPTS_R$verbose <- TRUE
    invisible()
  }

  verbose_off <- function() {
    OPTS_R$verbose <- FALSE
    invisible()
  }

  attr(debug_on, "desc")    <- paste(
    "Activates the debugging mode. When active, jobs will be submitted using sh",
    "and not sbatch. Also, only a single chunk of the data will be processed."
  )
  attr(debug_off, "desc")   <- "Deactivates the debugging mode."
  attr(verbose_on, "desc")  <- paste(
    "Deactivates the verbose mode. When ON, sbatch prints the Rscript and batch",
    "files on screen so that the user knows what will be submitted to Slurm.")
  attr(verbose_off, "desc") <- "Deactivates the verbose mode."

  # Final structure ------------------------------------------------------------
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
      get_debug    = structure(
        function() OPTS_R$debug, desc="Returns TRUE of debug mode is on"),
      verbose_on   = verbose_on,
      verbose_off  = verbose_off,
      get_verbose  = structure(
        function() OPTS_R$verbose, desc="Returns TRUE if verbose mode is on."),
      get_cmd      = structure(
        function() OPTS_R$cmd,
        desc =  "If debug mode is active, then it returns `sh`, otherwise `sbatch`"
      )
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
    cat("Debugging mode is currently active, which means that `sbatch` will use `sh`",
        " instead (to deactivate it use opts_sluRm$debug_off()).\n")

  invisible(x)

}

.onLoad <- function(libname, pkgname) {
  opts_sluRm$set_chdir(getwd())

  tmp <- tempfile("sluRm-job-", opts_sluRm$get_chdir())
  tmp <- gsub(".+(?=sluRm-job-)", "", tmp, perl = TRUE)

  opts_sluRm$set_job_name(tmp)
}

