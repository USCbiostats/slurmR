#' Source an R script as a Slurm job
#'
#' This function sources R scripts using Slurm by creating a batch script file
#' and submitting it via [sbatch].
#'
#' @param file Character. Path to the R script to source using Slurm.
#' @param job_name Character. Name of the job to be passed to `Slurm`.
#' @param tmp_path Character. Path to the directory where all the data (including
#' scripts) will be stored. Notice that this path must be accessible by all the
#' nodes in the network (See [opts_sluRm]).
#' @param sbatch_opt,rscript_opt List. Options to be passed via flags to
#' the bash file as `#SBATCH` and to `Rscript` respectively.
#' @param ... Further arguments to be passed to [sbatch].
#' @export
#'
sourceSlurm <- function(
  file,
  job_name    = NULL,
  tmp_path    = tempdir(),
  rscript_opt = list(vanilla = TRUE),
  sbatch_opt  = list(),
  ...
  ) {

  # Creating a script name, if it is not specified, then we use the name of the
  # file as the name of the job.
  if (is.null(job_name))
    job_name <- gsub(".+[/](?=[^/]+$)", "", file, perl=TRUE)

  file        <- normalizePath(file)
  script_path <- sprintf("%s/%s.slurm", tmp_path, job_name)

  x <- new_bash(
    filename = script_path,
    job_name = job_name,
    tmp_path = tmp_path
    )

  # Adding options
  sbatch_opts$`job-name` <- job_name
  x$add_SBATCH(sbatch_opt)

  # Finalizing by setting the R call
  x$Rscript(file = file, flags = rscript_opt)

  # Writing the script
  x$write()

  message("The created file can be found here:\n ", script_path)
  sbatch(script_path, ...)
}
