#' List loaded packages together with `lib.loc`
#' @noRd
list_loaded_pkgs <- function() {

  # Getting the name spaces
  pkgs <- rev(sessionInfo()$otherPkgs)

  # Session

  structure(
    lapply(pkgs, function(p) {
      gsub(sprintf("/%s/.+", p$Package), "/", attr(p, "file"))
    }),
    names = names(pkgs),
    class = "sluRm_loaded_packages"
  )

}

#' Creates an R script
#' @noRd
#' @param pkgs A named list of R packages to load.
rscript_header <- function(pkgs) {

  sprintf("library(%s, lib.loc = \"%s\")", names(pkgs), unlist(pkgs))

}

#' General purpose function to write R scripts
#' @noRd
new_rscript <- function(pkgs = list_loaded_pkgs()) {

  # Creating the environment
  env <- new.env(parent = emptyenv())

  # The first statement is the task id number
  env$dat <- rscript_header(pkgs)
  env$dat <- c(
    env$dat,
    ".slurmARRAY_ID <- as.integer(Sys.getenv(\"SLURM_ARRAY_TASK_ID\"))"
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
      sprintf("options_sluRm$set_job_path(\"%s\")", options_sluRm$get_job_path()),
      sprintf(
        "options_sluRm$set_job_name(\"%s\", overwrite = FALSE)",
        options_sluRm$get_job_name(check)
        ),
      sprintf(
        "saveRDS(ans, sluRm:::snames(\"rds\", .slurmARRAY_ID), compress=%s)",
        ifelse(compress, "TRUE", "FALSE")
        ),
      "cat('0', file = sluRm:::snames(\"fin\", .slurmARRAY_ID))"
      )

    invisible()
  }

  env$add_rds <- function(x, index = FALSE) {

    # Recursion
    if (length(x) > 1)
      return(invisible(sapply(x, env$add_rds, idx = idx)))

    # Writing the line
    line <- sprintf(
      ".slurm%s <- readRDS(\"%s/%s/%1$s.rds\")",
      x,
      options_sluRm$get_job_path(),
      options_sluRm$get_job_name()
      )

    if (idx)
      line <- paste0(line, "[.slurmINDICES[[.slurmARRAY_ID]]]")

    env$dat <- c(env$dat, line)

    invisible()

  }

  env$write <- function() {
    writeLines(env$dat, snames("r"))
  }

  structure(env, class = "sluRm_rscript")

}


print.sluRm_rscript <- function(x, ...) {

  cat(x$dat, sep = "\n")

  invisible(x)

}
