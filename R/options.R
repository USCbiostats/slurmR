
#' Get and set default options for `sbatch` and `slurmR` internals
#'
#' Most of the functions in the `slurmR` package use `tmp_path` and `job-name`
#' options to write and submit jobs to **Slurm**. These options have global
#' defaults that are set and retrieved using `opts_slurmR`.
#'
#' Whatever the path specified on `tmp_path`, all nodes should have access to it.
#' Moreover, it is recommended to use a path located in a high-performing drive.
#' See for example \url{https://hpcc.usc.edu/support/infrastructure/temporary-disk-space/}.
#'
#' The `tmp_path` directory is only created at the time that one of the functions
#' needs to I/O files. Job creation calls like [Slurm_EvalQ] and [Slurm_lapply]
#' do such.
#'
#' @details Current supported options are:
#'
#' Debugging mode
#'
#' - `debug_on : function ()` \Sexpr{attr(slurmR::opts_slurmR$debug_on, "desc")}
#'
#' - `debug_off : function ()` \Sexpr{attr(slurmR::opts_slurmR$debug_off, "desc")}
#'
#' - `get_debug : function ()` \Sexpr{attr(slurmR::opts_slurmR$get_debug, "desc")}
#'
#' Verbose mode
#'
#' - `verbose_on : function ()` \Sexpr{attr(slurmR::opts_slurmR$verbose_on, "desc")}
#'
#' - `verbose_off : function ()` \Sexpr{attr(slurmR::opts_slurmR$verbose_off, "desc")}
#'
#' - `get_verbose : function ()` \Sexpr{attr(slurmR::opts_slurmR$get_verbose, "desc")}
#'
#' Slurm options
#'
#' - `set_tmp_path : function (path, recursive = TRUE)` \Sexpr{attr(slurmR::opts_slurmR$set_tmp_path, "desc")}
#'
#' - `get_tmp_path : function ()` \Sexpr{attr(slurmR::opts_slurmR$get_tmp_path, "desc")}
#'
#' - `set_job_name : function (path, check = TRUE, overwrite = TRUE)` \Sexpr{attr(slurmR::opts_slurmR$set_job_name, "desc")}.
#'
#' - `get_job_name : function (check = TRUE)` \Sexpr{attr(slurmR::opts_slurmR$get_job_name, "desc")}
#'
#' Other options
#'
#' - `get_cmd : function ()` \Sexpr{attr(slurmR::opts_slurmR$get_cmd, "desc")}
#'
#'
#' For general set/retrieve options
#'
#' - `set_opts : function (...)` \Sexpr{attr(slurmR::opts_slurmR$set_opts, "desc")}
#'
#' - `get_opts_job : function (...)` \Sexpr{attr(slurmR::opts_slurmR$get_opts_job, "desc")}
#'
#' - `get_opts_r : function (...)` \Sexpr{attr(slurmR::opts_slurmR$get_opts_r, "desc")}
#'
#'
#' @examples
#'
#' # Common setup
#' \dontrun{
#' opts_slurmR$set_tmp_path("/staging/pdt/vegayon")
#' opts_slurmR$set_job_name("simulations-1")
#' opts_slurm$set_opts(partition="thomas", account="lc_pdt")
#' }
#'
#' @export
opts_slurmR <- (function() {

  # Default chdir is null and will be set at the first call of the function
  OPTS_SLURM            <- new.env(parent = emptyenv())
  OPTS_SLURM$`job-name` <- NULL

  OPTS_R          <- new.env(parent = emptyenv())
  OPTS_R$tmp_path <- NULL
  OPTS_R$debug    <- FALSE
  OPTS_R$cmd      <- "sbatch"
  OPTS_R$verbose  <- FALSE


  # JOB PATH -------------------------------------------------------------------
  # Function to set job path
  set_tmp_path <- function(path, recursive = TRUE, overwrite = FALSE) {

    # Path normalization
    path <- normalizePath(path)

    # Check if we are renewing the filepath.
    if (length(OPTS_SLURM$`job-name`)) {

      fn <- sprintf("%s/%s", path, get_job_name())
      if (dir.exists(fn) && !overwrite)
        stop(
          "The folder '", fn, "' already exists. In order to overwrite it you ",
          "must set the option `overwrite = TRUE`.", call. = FALSE
        )

    }

    # Recursively creating the folder
    if (!dir.exists(path)) {
      dir.create(path, recursive = recursive)
    }

    OPTS_R$tmp_path <- path

    invisible()
  }

  # Function to get the path
  get_tmp_path <- function() {
    if (!length(OPTS_R$tmp_path))
      OPTS_R$tmp_path <- getwd()
    OPTS_R$tmp_path
  }

  attr(set_tmp_path, "desc") <- "Sets the tempfile path for I/O"
  attr(get_tmp_path, "desc") <- "Retrieves tempfile path for I/O"

  # JOB NAME -------------------------------------------------------------------
  set_job_name <- function(name, overwrite = TRUE) {

    if (!length(name))
      stop("The `name` cannot be NULL", call. = FALSE)
    else if (name == "")
      stop(
        "`name` must be a meaningful name. Cannot be \"\" (empty).",
        call. = FALSE
        )

    fn <- sprintf("%s/%s", get_tmp_path(), name)
    if (overwrite && file.exists(fn)) {

      warning(
        "The folder '", fn, "' already exists and will be overwritten.",
        call. = FALSE
        )

      status <- file.remove(list.files(fn, full.names = TRUE))

    } else if (!overwrite && file.exists(fn))
      stop(
        "The folder '", fn, "' already exists. In order to overwrite it you ",
        "must set the option `overwrite = TRUE`.", call. = FALSE
        )

    OPTS_SLURM$`job-name` <- name
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
      warning("The following options can be set via `opts_slurmR$set_*`: `",
              paste0(test, collapse="`, `"), "`.", call. = FALSE)

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

  # Getting Slurm options
  get_opts_job <- function(...) {

    dots <- list(...)
    opts <- as.list(OPTS_SLURM)
    opts <- opts[!sapply(opts, is.null)]

    if (!length(dots)) {
      return(opts)
    }

    dots <- unlist(dots)

    if (any(!is.character(dots)))
      stop("`...` only receives characters.", call. = FALSE)

    opts[intersect(dots, names(opts))]

  }

  # Getting R options
  get_opts_r <- function(...) {

    dots <- list(...)
    if (!length(dots))
      return(as.list(OPTS_R))

    dots <- unlist(dots)

    if (any(!is.character(dots)))
      stop("`...` only receives characters.", call. = FALSE)

    as.list(OPTS_R)[intersect(dots, names(OPTS_R))]

  }

  attr(set_opts, "desc")     <- "A generic function to set options."
  attr(get_opts_r, "desc")   <- "A generic function to retrieve options in R."
  attr(get_opts_job, "desc") <- "A generic function to retrieve options for the job (Slurm)."

  # Debugging and Verbose ------------------------------------------------------

  debug_on <- function() {
    OPTS_R$debug   <- TRUE
    OPTS_R$cmd     <- "sh"
    OPTS_R$verbose <- TRUE
    message("Debug mode is now active. Which means that jobs will be called via",
            " `sh` and not `sbatch`. You can de-activate debug mode by calling",
            " opts_slurmR$debug_off(). Notice that only 1/njobs will be submitted",
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
      set_tmp_path    = set_tmp_path,
      get_tmp_path    = get_tmp_path,
      set_job_name = set_job_name,
      get_job_name = get_job_name,
      set_opts     = set_opts,
      get_opts_job = get_opts_job,
      get_opts_r   = get_opts_r,
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
  ), class = "opts_slurmR")

})()

#' Fill options for slurm using slurmr's defaults (if any)
#' @noRd
#'
coalesce_slurm_options <- function(x, y = opts_slurmR$get_opts_job()) {

  for (i in names(y)) {
    if (i %in% names(x))
      next
    x[[i]] <- y[[i]]
  }

  return(x)
}

#' @export
print.opts_slurmR <- function(x, ...) {

  options_printer <- function(x) {
    cat(sprintf("  %-12s: %s", names(x), as.character(x)), sep="\n")
  }

  cat("\nOptions for sbatch (Slurm workflow):\n")
  options_printer(x$get_opts_job())
  cat("\nOther options (R workflow):\n")
  options_printer(x$get_opts_r())
  cat("\nTo get and set options for Slurm jobs creation use (see ?opts_slurmR):\n\n")
  print(utils::ls.str(x))

  if(x$get_debug())
    cat("Debugging mode is currently active, which means that `sbatch` will use `sh`",
        " instead (to deactivate it use opts_slurmR$debug_off()).\n")
  else cat("\n")

  invisible(x)

}

