
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
rscript_header <- function(pkgs = list_loaded_pkgs()) {

  structure(
    sprintf("library(%s, lib.loc = \"%s\")", names(pkgs), unlist(pkgs)),
    class = c("sluRm_plaintext", "sluRm_rscript")
  )


}

#' @export
print.sluRm_plaintext <- function(x, ...) {
  cat(x, sep="\n")
  invisible(x)
}

#' @export
c.sluRm_plaintext <- function(...) {

  structure(
    .Data = do.call(c, lapply(list(...), unclass)),
    class = "sluRm_plaintext"
  )

}

write_rbash <- function() {

}

bash_header <- function() {

}

#' Functio to write out a bash file calling R for slurm
#' @param nodes Integer, number of nodes to specify.
#' @param Rscript_flags Character specifying flags to pass to Rscript.
#' @param ... List of arguments passed to `SBATCH` (see details).
#'
#' @details
#' The `...` argument allows passing options via `SBATCH` in the script, for
#' example, if the user wants to use account `user1` and require a minimum of
#' 2 CPUS per node, the user can pass the argument:
#'
#' ```
#' list(account="user1", mincpus=2)
#' ```
#'
#' Which will translate into
#'
#' ```
#' #SBATCH --account="user1"
#' #SBATCH --mincpus=2
#' ```
#'
#' In the bash file. Available options can be found
#' https://slurm.schedmd.com/sbatch.html#OPTIONS.
#' @noRd
write_bash <- function(
  nodes         = 2,
  Rscript_flags = "--vanilla",
  ...
  ) {

  # Collecting extra arguments
  dots <- c(
    list(...),
    list(
      `job-name` = options_sluRm$get_job_name(),
      output     = snames("out"),
      array      = sprintf("1-%i", nodes))
    )

  # Adding quotation
  dots <- lapply(dots, function(d) {
    if (is.character(d))
      paste0("\"", d,"\"")
    else d
    })

  # Creating the bash
  SBATCH <- if (length(dots))
    sprintf("#SBATCH --%s=%s", names(dots), unlist(dots))
  else
    NULL

  # Putting everything together
  structure(
    c(
      "#!/bin/sh",
      SBATCH,
      sprintf("%s/bin/Rscript %s %s", R.home(), Rscript_flags, snames("r"))
    ),
    class = c("sluRm_plaintext", "sluRm_bash")
  )

}

save_objects <- function(
  objects,
  ...
  ) {

  # Creating and checking  path
  path <- paste(
    options_sluRm$get_job_path(),
    options_sluRm$get_job_name(),
    sep="/"
    )

  # Saving objects
  Map(
    function(n, x) saveRDS(x, n, ...),
    x = objects,
    n = sprintf("%s/%s.rds", path, names(objects))
  )

  names(objects)

}



# write_bash("x.r", array="0-1")
