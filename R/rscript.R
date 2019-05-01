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
    class = "sluRm_loaded_packages"
  )

}

#' Creates an R script
#' @noRd
#' @param pkgs A named list of R packages to load.
rscript_header <- function(pkgs, seeds = NULL) {

  # For testing purposes, the instalation of the package is somewhere else
  intestthat <- identical(Sys.getenv("TESTTHAT"), "true")

  if (intestthat)
    pkgs[names(pkgs) == "sluRm"] <- NULL

  ans <- sprintf("library(%s, lib.loc = \"%s\")", names(pkgs), unlist(pkgs))

  if (intestthat)
    c(ans, "library(sluRm)")
  else
    ans

}

#' General purpose function to write R scripts
#'
#' This function will create an object of class `sluRm_rscript` that can be used
#' to write the R component in a batch job.
#' @param pkgs A named list with packages to be included. Each element of the list
#' must be a path to the R library, while the names of the list are the names of
#' the R packages to be loaded.
#'
#' @return An environment of class `sluRm_rscript`. This has the following accesible
#' components:
#'
#' - `add_rds` This function is used to
#' - `append`
#' - `dat`
#' - `finalize`
#' - `set_seed`
#' - `write`
#'
#' @noRd
new_rscript <- function(pkgs = list_loaded_pkgs()) {

  # Creating the environment
  env <- new.env(parent = emptyenv())

  # The first statement is the task id number
  env$dat <- rscript_header(pkgs)
  env$dat <- c(
    env$dat,
    sprintf("%-16s <- as.integer(Slurm_env(\"SLURM_ARRAY_TASK_ID\"))", "ARRAY_ID")
    )

  # Function to append a line
  env$append <- function(x) {

    # In case of multiple statements
    if (length(x) > 1)
      return(invisible(sapply(x, env$append)))

    env$dat <- c(env$dat, x)
    invisible()
  }

  # Function to finalize the Rscript
  env$finalize <- function(check = TRUE, compress = TRUE) {

    env$dat <- c(
      env$dat,
      sprintf("opts_sluRm$set_chdir(\"%s\")", opts_sluRm$get_chdir()),
      sprintf(
        "opts_sluRm$set_job_name(\"%s\", overwrite = FALSE)",
        opts_sluRm$get_job_name(check)
        ),
      sprintf(
        "saveRDS(ans, sluRm::snames(\"rds\", ARRAY_ID), compress=%s)",
        ifelse(compress, "TRUE", "FALSE")
        )
      )

    invisible()
  }

  env$add_rds <- function(x, index = FALSE) {

    # Recursion
    if (length(x) > 1)
      return(invisible(sapply(x, env$add_rds, index = index)))

    # Writing the line
    line <- sprintf(
      "%-16s <- readRDS(\"%s/%s/%1$s.rds\")",
      x,
      opts_sluRm$get_chdir(),
      opts_sluRm$get_job_name()
      )

    if (index)
      line <- paste0(line, "[INDICES[[ARRAY_ID]]]")

    env$dat <- c(env$dat, line)

    invisible()

  }

  attr(env$add_rds, "desc") <- paste(
    "Add rds files to be loaded in each job.", "`x` is a character vector with",
    "the names of the files (without extension or path) that will be added.",
    "If `index = TRUE` the function assumes that the user will be accessing",
    "a particular subset of `x` during the job, which is accessed according to",
    "`INDICES[[ARRAY_ID]]`.")

  env$set_seed <- function(x, kind = NULL, normal.kind = NULL) {

    # Reading the seeds
    invisible(save_objects(list(seeds = x)))
    env$add_rds("seeds", index = FALSE)
    line <- sprintf("set.seed(seeds[ARRAY_ID], kind = %s, normal.kind = %s)",
                    ifelse(length(kind), kind, "NULL"),
                    ifelse(length(normal.kind), normal.kind, "NULL"))
    env$dat <- c(env$dat, line)

    invisible()

  }

  attr(env$set_seed, "desc") <- "ABC"

  env$write <- function() {
    writeLines(env$dat, snames("r"))
  }

  structure(env, class = "sluRm_rscript")

}


print.sluRm_rscript <- function(x, ...) {

  cat(x$dat, sep = "\n")

  invisible(x)

}

