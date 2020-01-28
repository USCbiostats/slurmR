#' List loaded packages together with `lib.loc`
#' @noRd
list_loaded_pkgs <- function() {

  # Getting the name spaces
  pkgs <- rev(sessionInfo()$otherPkgs)

  # Session

  structure(
    lapply(pkgs, function(p) {
      gsub(sprintf("/%s/.+", p$Package), "", attr(p, "file"))
    }),
    names = names(pkgs),
    class = "slurmR_loaded_packages"
  )

}

tryCatch_and_quit <- function(...) {

  ans <- tryCatch(..., error = function(e) e)
  if (inherits(ans, "error")) {

    saveRDS(
      ans,
      snames(
        "rds",
        tmp_path = get("TMP_PATH", envir = .GlobalEnv),
        job_name = get("JOB_NAME", envir = .GlobalEnv),
        array_id = get("ARRAY_ID", envir = .GlobalEnv)
        )
      )

    q("no")
  }
  ans

}

#' Creates an R script
#' @noRd
#' @param pkgs A named list of R packages to load.
load_packages <- function(pkgs, tmp_path, job_name) {

  # For testing purposes, the instalation of the package is somewhere else
  if ("slurmR" %in% names(pkgs))
    pkgs[names(pkgs) == "slurmR"] <- NULL

  sprintf("tryCatch_and_quit(library(%s, lib.loc = \"%s\"))", names(pkgs), unlist(pkgs))

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
  env$append <- function(x) {

    # In case of multiple statements
    if (length(x) > 1)
      return(invisible(sapply(x, env$append)))

    env$rscript <- c(env$rscript, x)
    invisible()
  }

  # Checking libpaths
  if (length(libPaths)) {

    if (is.atomic(libPaths) && is.character(libPaths)) {
      env$append(sprintf(
        ".libPaths(c(\"%s\"))",
        paste(libPaths, collapse="\", \"")
        ))
    } else {

      stop("The argument `libPaths` must be either a vector or a character scalar.",
           call. = FALSE)

    }

  }

  # Constants
  env$append(
    c(
      sprintf("Slurm_env <- %s", paste(deparse(Slurm_env), collapse="\n")),
      sprintf("TMP_PATH  <- \"%s\"", tmp_path),
      sprintf("JOB_NAME  <- \"%s\"", job_name),
      paste0("tryCatch_and_quit <- ", paste(deparse(tryCatch_and_quit), collapse = "\n"))
      )
    )

  env$append(sprintf("%-16s <- as.integer(Slurm_env(\"SLURM_ARRAY_TASK_ID\"))", "ARRAY_ID"))
  env$append(load_packages(pkgs, tmp_path = tmp_path, job_name = job_name))

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
    ))

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
          "tryCatch_and_quit(%-16s <- readRDS(sprintf(\"%s/%s/%1$s_%%04d.rds\", ARRAY_ID)))"
        } else {
          "tryCatch_and_quit(%-16s <- readRDS(\"%s/%s/%1$s.rds\"))"
        },
        names(x)[i],
        tmp_path,
        job_name
      )

      # if (split)
      #   line <- paste0(line, "[INDICES[[ARRAY_ID]]]")

      env$rscript  <- c(env$rscript, line)
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

