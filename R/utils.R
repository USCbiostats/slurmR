
#' List loaded packages together with `lib.loc`
#' @noRd
list_loaded_pkgs <- function() {

  # Getting the name spaces
  pkgs <- rev(loadedNamespaces())

  # Session

  structure(
    lapply(pkgs, function(p) {
      p <- packageDescription(p)
      gsub(sprintf("/%s/Meta/package[.]rds$", p$Package), "", attr(p, "file"))
    }),
    names = pkgs,
    class = "sluRm_loaded_packages"
  )

}

#' Creates an R script
#' @param pkgs A named list of R packages to load.
rscript_header <- function(pkgs = list_loaded_pkgs()) {

  structure(
    sprintf("library(%s, lib.loc = \"%s\")", names(pkgs), unlist(pkgs)),
    class = c("sluRm_plaintext", "sluRm_rscript")
  )


}

print.sluRm_plaintext <- function(x, ...) {
  cat(x, sep="\n")
  invisible(x)
}

write_rbash <- function() {

}

bash_header <- function() {

}

#' Functio to write out a bash file calling R for slurm
#' @param file Full path to the R script to be submitted to Slurm.
#' @param Rscript_flags Character specifying flags to pass to Rscript.
#' @param job_name Character. Name of the job to be passed via `SBATCH`.
#' @param output Character. Format of the output file to be passed to `SBATCH`.
#'
#' @details  See
#' @export
write_bash <- function(
  file,
  job_path,
  job_name      = getOption("sluRm.job_name", "sluRm"),
  Rscript_flags = "--vanilla",
  output        = sprintf("%s-%%a.out", job_name),
  ...
  ) {

  # Collecting extra arguments
  dots <- c(list(...), list(`job-name` = job_name, output = output))
  SBATCH <- if (length(dots))
    sprintf("#SBATCH --%s=%s", names(dots), unlist(dots))
  else
    NULL

  # Checking whether the file exists or not
  if (!file.exists(file))
    stop("The file `", file, "` does not exists.", call. = FALSE)

  # Putting everything together
  structure(
    c(
      "#!/bin/sh",
      SBATCH,
      sprintf("%sscript %s %s", Sys.getenv("R_HOME"), Rscript_flags, file)
    ),
    class = c("sluRm_plaintext", "sluRm_bash")
  )

}

save_objects <- function(
  objects,
  job_path,
  job_name = getOption("sluRm.job_name", "sluRm"),
  ...
  ) {

  # Creating and checking  path
  path <- paste(job_path, job_name, sep="/")
  if (!file.exists(path))
    dir.create(path)

  # Saving objects
  Map(
    function(n, x) saveRDS(x, n, ...),
    x = objects,
    n = sprintf("%s/%s.rds", path, names(objects))
  )

  names(objects)

}

# write_bash("x.r", array="0-1")
