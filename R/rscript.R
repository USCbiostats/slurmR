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
rscript_header <- function(pkgs, seeds = NULL) {

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
    sprintf(".s%-16s <- as.integer(Slurm_env(\"SLURM_ARRAY_TASK_ID\"))", "ARRAY_ID")
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
        "saveRDS(ans, sluRm::snames(\"rds\", .sARRAY_ID), compress=%s)",
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
      ".s%-16s <- readRDS(\"%s/%s/%1$s.rds\")",
      x,
      opts_sluRm$get_chdir(),
      opts_sluRm$get_job_name()
      )

    if (index)
      line <- paste0(line, "[.sINDICES[[.sARRAY_ID]]]")

    env$dat <- c(env$dat, line)

    invisible()

  }

  env$set_seed <- function(x, kind = NULL, normal.kind = NULL) {

    # Reading the seeds
    env$add_rds("seeds", index = TRUE)
    line <- sprintf("set.seed(.sseeds[.sARRAY_ID], kind = %s, normal.kind = %s)",
                    ifelse(length(kind), kind, "NULL"),
                    ifelse(length(normal.kind), normal.kind, "NULL"))
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

