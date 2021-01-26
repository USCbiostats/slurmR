
#' Get and set default options for `sbatch` and `slurmR` internals
#'
#' Most of the functions in the `slurmR` package use `tmp_path` and `job-name`
#' options to write and submit jobs to **Slurm**. These options have global
#' defaults that are set and retrieved using `opts_slurmR`. These options
#' also include SBATCH options and things to do before calling RScript,
#' e.g., loading modules on an HPC cluster.
#'
#' Whatever the path specified on `tmp_path`, all nodes should have access to it.
#' Moreover, it is recommended to use a path located in a high-performing drive.
#' See for example [disk staging](https://en.wikipedia.org/w/index.php?title=Disk_staging&oldid=908353920).
#'
#' The `tmp_path` directory is only created at the time that one of the functions
#' needs to I/O files. Job creation calls like [Slurm_EvalQ] and [Slurm_lapply]
#' do such.
#'
#' The "preamble" options can be specified if, for example, the current cluster
#' needs to load R, a compiler, or other programs via a `module` command.
#'
#' @details Current supported options are:
#'
#' Debugging mode
#'
#' - `debug_on : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$debug_on, "desc")}
#'
#' - `debug_off : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$debug_off, "desc")}
#'
#' - `get_debug : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_debug, "desc")}
#'
#' Verbose mode
#'
#' - `verbose_on : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$verbose_on, "desc")}
#'
#' - `verbose_off : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$verbose_off, "desc")}
#'
#' - `get_verbose : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_verbose, "desc")}
#'
#' Slurm options
#'
#' - `set_tmp_path : function (path, recursive = TRUE)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$set_tmp_path, "desc")}
#'
#' - `get_tmp_path : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_tmp_path, "desc")}
#'
#' - `set_job_name : function (path, check = TRUE, overwrite = TRUE)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$set_job_name, "desc")}.
#'
#' - `get_job_name : function (check = TRUE)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_job_name, "desc")}
#'
#' Other options
#'
#' - `get_cmd : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_cmd, "desc")}
#'
#' - `set_preamble : function (...)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$set_preamble, "desc")}
#'
#' - `get_preamble : function ()` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_preamble, "desc")}
#'
#' For general set/retrieve options
#'
#' - `set_opts : function (...)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$set_opts, "desc")}
#'
#' - `get_opts_job : function (...)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_opts_job, "desc")}
#'
#' - `get_opts_r : function (...)` \Sexpr[stage=build]{attr(slurmR::opts_slurmR$get_opts_r, "desc")}
#'
#'
#' @examples
#'
#' # Common setup
#' \dontrun{
#' opts_slurmR$set_tmp_path("/staging/pdt/vegayon")
#' opts_slurmR$set_job_name("simulations-1")
#' opts_slurm$set_opts(partition="thomas", account="lc_pdt")
#' opts_slurm$set_preamble("module load gcc")# if needed
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

  # Preamble
  OPTS_PREAMBLE     <- new.env(parent = emptyenv())
  OPTS_PREAMBLE$dat <- NULL

  # JOB PATH -------------------------------------------------------------------
  # Function to set job path
  set_tmp_path <- function(path) {

    # Path normalization and assignment
    path <- normalizePath(path)
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
  set_job_name <- function(name) {

    if (!length(name))
      stop("The `name` cannot be NULL", call. = FALSE)
    else if (name == "")
      stop(
        "`name` must be a meaningful name. Cannot be \"\" (empty).",
        call. = FALSE
        )

    fn <- sprintf("%s/%s", get_tmp_path(), name)

    OPTS_SLURM$`job-name` <- name
    invisible()

  }

  get_job_name <- function(check = TRUE) {

    if (!length(OPTS_SLURM$`job-name`))
      OPTS_SLURM$`job-name` <- random_job_name()

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

  get_preamble <- function() {
    return(OPTS_PREAMBLE$dat)
  }

  set_preamble <- function(...) {
    OPTS_PREAMBLE$dat <- c(OPTS_PREAMBLE$dat, unlist(list(...)))
    invisible()
  }

  attr(set_preamble, "desc") <- paste(
    "Sets \"preamble\" to the RScript call. For example, it could be used for loading",
    "modules, setting env variables, etc., needed during the R session. Options are",
    "passed as characters."
  )
  attr(get_preamble, "desc") <- paste(
    "Returns the preamble, e.g., module loads, environment variable definitions, etc.,",
    "that will be included in sbatch submissions."
  )

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
      set_tmp_path = set_tmp_path,
      get_tmp_path = get_tmp_path,
      set_job_name = set_job_name,
      get_job_name = get_job_name,
      set_opts     = set_opts,
      get_opts_job = get_opts_job,
      get_opts_r   = get_opts_r,
      set_preamble = set_preamble,
      get_preamble = get_preamble,
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
  options_printer(list(preamble = x$get_preamble()))
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

