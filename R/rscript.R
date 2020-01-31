#' List loaded packages together with `lib.loc`
#' @noRd
list_loaded_pkgs <- function(exclude_slurmR = TRUE) {

  # Getting the name spaces
  pkgs <- rev(sessionInfo()$otherPkgs)

  # Session
  ans <- structure(
    lapply(pkgs, function(p) {
      gsub(sprintf("/%s/.+", p$Package), "", attr(p, "file"))
    }),
    names = names(pkgs)
  )

  if (exclude_slurmR)
    return(ans[setdiff(names(ans), "slurmR")])

  ans

}

tcq <- function(...) {

  ans <- tryCatch(..., error = function(e) e)
  if (inherits(ans, "error")) {

    ARRAY_ID. <- get("ARRAY_ID", envir = .GlobalEnv)
    msg <- paste(
      "An error has ocurred while evualting the expression:\n",
      paste(deparse(match.call()[[2]]), collapse = "\n"), "\n in ",
      "ARRAY_ID # ", ARRAY_ID.
    )
    warning(msg, immediate. = TRUE, call. = FALSE)

    ans$message <- paste(ans$message, msg)

    saveRDS(
      ans,
      snames(
        "rds",
        tmp_path = get("TMP_PATH", envir = .GlobalEnv),
        job_name = get("JOB_NAME", envir = .GlobalEnv),
        array_id = ARRAY_ID.
        )
      )

    q("no")
  }
  invisible(ans)

}

#' Creates an R script
#' @noRd
#' @param pkgs A named list of R packages to load.
load_packages <- function(pkgs, tmp_path, job_name) {

  # For testing purposes, the instalation of the package is somewhere else
  sprintf("library(%s, lib.loc = \"%s\")", names(pkgs), unlist(pkgs))

}

save_objects <- function(
  objects,
  tmp_path,
  job_name,
  compress = TRUE,
  njobs    = NULL,
  ...
) {

  # Creating and checking  path
  path <- sprintf("%s/%s", tmp_path, job_name)

  # Saving objects
  for (i in seq_along(objects)) {

    if (!length(njobs)) {

      saveRDS(
        objects[[i]],
        sprintf("%s/%s.rds", path, names(objects)[i]),
        compress = compress
      )

    } else {

      # Getting the splits
      splits <- parallel::splitIndices(length(objects[[i]]), njobs)

      for (j in seq_len(njobs)) {

        saveRDS(
          objects[[i]][splits[[j]]],
          sprintf("%s/%s_%04d.rds", path, names(objects)[i], j),
          compress = compress
        )

      }

    }
  }

  invisible()

}

#' General purpose function to write R scripts
#'
#' This function will create an object of class `slurmR_rscript` that can be used
#' to write the R component in a batch job.
#' @template njobs
#' @param pkgs A named list with packages to be included. Each element of the list
#' must be a path to the R library, while the names of the list are the names of
#' the R packages to be loaded.
#' @param libPaths A character vector. See [.libPaths].
#' @template job_name-tmp_path
#'
#' @return An environment of class `slurmR_rscript`. This has the following accessible
#' components:
#'
#' - `add_rds` Add rds files to be loaded in each job.", `x` is a named list
#'   with the objects that should be loaded in the jobs. If `index = TRUE` the
#'   function assumes that the user will be accessing a particular subset of `x`
#'   during the job, which is accessed according to `INDICES[[ARRAY_ID]]`. The
#'   option `compress` is passed to [saveRDS].
#'
#'   One important side effect is that when this function is called, the object
#'   will be saved in the current job directory, this is `opts_slurmR$get_tmp_path()`.
#'
#' - `append` Adds a line to the R script. Its only argument, `x` is a character
#'   vector that will be added to the R script.
#'
#' - `rscript` A character vector. This is the actual R script that will be written
#'   at the end.
#'
#' - `finalize` Adds the final line of the R script. This function receives a
#'   character scalar `x` which is used as the name of the object to be saved.
#'   If missing, the function will save a NULL object. The `compress` argument
#'   is passed to [saveRDS].
#'
#' - `set_seed` Adds a vector of seeds to be used across the jobs. This vector
#'   of seeds should be of length `njobs`. The other two parameters of the
#'   function are passed to [set.seed]. By default the seed is picked as follows:
#'
#'   ```
#'   seeds <- sample.int(.Machine$integer.max, njobs, replace = FALSE)
#'   ```
#'
#' - `write` Finalizes the process by writing the R script in the corresponding
#'   folder to be used with Slurm.
#'
#' @export
new_rscript <- function(
  njobs,
  tmp_path,
  job_name,
  pkgs     = list_loaded_pkgs(),
  libPaths = .libPaths()
  ) {

  # Creating the environment
  env <- new.env(parent = emptyenv())

  # The first statement is the task id number
  env$rscript <- NULL
  env$njobs   <- njobs

  # Function to append a line
  env$append <- function(x, wrap = TRUE) {

    x <- paste0(
      ifelse(wrap, "tcq({\n  ", ""),
      paste(x, collapse = ifelse(wrap, "\n  ", "\n")),
      ifelse(wrap, "\n})", "")
      )

    env$rscript <- c(env$rscript, x)
    invisible()
  }

  # Checking libpaths
  if (length(libPaths)) {

    if (is.atomic(libPaths) && is.character(libPaths)) {
      env$append(sprintf(
        ".libPaths(c(\"%s\"))",
        paste(libPaths, collapse="\", \"")
        ), wrap = FALSE)
    } else {

      stop("The argument `libPaths` must be either a vector or a character scalar.",
           call. = FALSE)

    }

  }

  # Constants
  env$append(
    c(
      "message(\"[slurmR info] Loading variables and functions... \", appendLF = FALSE)",
      sprintf("Slurm_env <- %s", paste(deparse(Slurm_env), collapse = "\n")),
      "ARRAY_ID  <- as.integer(Slurm_env(\"SLURM_ARRAY_TASK_ID\"))",
      "\n# The -snames- function creates the write names for I/O of files as a ",
      "# function of the ARRAY_ID",
      sprintf("snames    <- %s", paste(deparse(snames), collapse = "\n")),
      sprintf("TMP_PATH  <- \"%s\"", tmp_path),
      sprintf("JOB_NAME  <- \"%s\"", job_name),
      "\n# The -tcq- function is a wrapper of tryCatch that on error tries to recover",
      "# the message and saves the outcome so that slurmR can return OK.",
      paste0("tcq <- ", paste(deparse(tcq), collapse = "\n")),
      "message(\"done loading variables and functions.\")"
      ),
    wrap = FALSE
    )

  # We only load packages if loaded...
  pkgs <- load_packages(pkgs, tmp_path = tmp_path, job_name = job_name)
  if (length(pkgs)) {
    env$append("message(\"[slurmR info] Loading packages ... \")", wrap = FALSE)
    env$append(pkgs)
    env$append("message(\"[slurmR info] done loading packages.\")", wrap = FALSE)
  }

  # Function to finalize the Rscript
  env$finalize <- function(x, compress = TRUE) {

    env$append(
      sprintf(
        "saveRDS(%s, %s, compress = %s)",
        if (missing(x)) "NULL" else x,
        sprintf(
          "sprintf(\"%s\", ARRAY_ID)",
          snames("rds", tmp_path = tmp_path, job_name = job_name)
          ),
        ifelse(compress, "TRUE", "FALSE")
    ), wrap = FALSE)

    invisible()
  }

  env$add_rds <- function(x, split = FALSE, compress = TRUE) {

    # Checking
    if (!is.list(x))
      stop("`x` must be a list.", call. = FALSE)
    if (!length(names(x)))
      stop("`x` must be a named list.", call. = FALSE)

    # Saving the objects
    save_objects(
      x,
      job_name = job_name,
      tmp_path = tmp_path,
      njobs    = if (split) njobs else NULL,
      compress = compress
      )

    for (i in seq_along(x)) {
      # Writing the line
      line <- sprintf(
        if (split) {
          "%s <- readRDS(sprintf(\"%s/%s/%1$s_%%04d.rds\", ARRAY_ID))"
        } else {
          "%s <- readRDS(\"%s/%s/%1$s.rds\")"
        },
        names(x)[i],
        tmp_path,
        job_name
      )

      env$append(line)
      env$robjects <- c(env$robjects, names(x)[i])
    }

    invisible()

  }

  env$set_seed <- function(x = NULL, kind = NULL, normal.kind = NULL) {

    # Reading the seeds
    if (is.null(x))
      x <- sample.int(.Machine$integer.max, env$njobs, replace = FALSE)

    env$add_rds(list(seeds = x), split = FALSE)
    line <- sprintf("set.seed(seeds[ARRAY_ID], kind = %s, normal.kind = %s)",
                    ifelse(length(kind), kind, "NULL"),
                    ifelse(length(normal.kind), normal.kind, "NULL"))

    # Don't want the seed to be kept in the list
    env$robjects <- env$robjects[1L:(length(env$robjects) - 1L)]
    env$rscript <- c(env$rscript, line)

    invisible()

  }

  env$write <- function() {
    writeLines(
      env$rscript,
      snames("r", tmp_path = tmp_path, job_name = job_name)
      )
  }

  structure(env, class = "slurmR_rscript")

}


print.slurmR_rscript <- function(x, ...) {

  cat(x$rscript, sep = "\n")

  invisible(x)

}

