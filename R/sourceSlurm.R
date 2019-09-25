#' Source an R script as a Slurm job
#'
#' This function sources R scripts using Slurm by creating a batch script file
#' and submitting it via [sbatch].
#'
#' @param file Character. Path to the R script to source using Slurm.
#' @template job_name-tmp_path
#' @template rscript_opt
#' @param ... Further options passed to [sbatch].
#'
#' @details
#' `sourceSlurm` checks for flags that may be included in the Slurm job file. If
#' the R script starts with `#!/bin/` or similar, then `#SBATCH` flags will be
#' read from the R script and added to the Slurm job file.
#'
#' @return Whatever [sbatch] returns.
#' @export
#' @examples
#' # In this example we will be sourcing an R script that also has #SBATCH
#' # flags. Here are the contents
#' file <- system.file("example.R", package="sluRm")
#'
#' cat(readLines(file), sep="\n")
#' # #!/bin/sh
#' # #SBATCH --account=lc_ggv
#' # #SBATCH --time=01:00:00
#' # #SBATCH --mem-per-cpu=4G
#' # #SBATCH --job-name=Waiting
#' # Sys.sleep(10)
#' # message("done.")
#'
#' # We can directly submit this R script as a job by calling `sourceSlurm`.
#' # (of course you need Slurm to do this!)
#' \dontrun{
#' sourceSlurm(file)
#' }
#'
#' # The function will create a bash script that is used later to be submitted to
#' # the queue using `sbatch`. The resulting file looks something like this
#' # #!/bin/sh
#' # #SBATCH --job-name=Waiting
#' # #SBATCH --output=/home/vegayon/Documents/sluRm/Waiting.out
#' # #SBATCH --account=lc_ggv
#' # #SBATCH --time=01:00:00
#' # #SBATCH --mem-per-cpu=4G
#' # /usr/lib/R/bin/Rscript --vanilla /usr/local/lib/R/site-library/sluRm/example.R
sourceSlurm <- function(
  file,
  job_name    = NULL,
  tmp_path    = tempdir(),
  rscript_opt = list(vanilla = TRUE),
  ...
  ) {

  # Checking that the file exists
  if (!file.exists(file))
    stop("File ", file, " not found.", call. = FALSE)

  # Reading in the first comments
  SBATCH <- readLines(file)
  SBATCH <- if (grepl("^#!/bin/", SBATCH[1])) {
    as.list(read_sbatch(SBATCH))
  } else
    list()

  # Creating a script name, if it is not specified, then we use the name of the
  # file as the name of the job.
  if (!is.null(job_name))
    SBATCH$`job-name` <- job_name
  if (is.null(SBATCH$`job-name`) & is.null(job_name)) {
    job_name          <- gsub(".+[/](?=[^/]+$)", "", file, perl=TRUE)
    SBATCH$`job-name` <- job_name
  }
  if (!is.null(SBATCH$`job-name`) & is.null(job_name))
    job_name <- SBATCH$`job-name`

  file        <- normalizePath(file)
  script_path <- sprintf("%s/%s.slurm", tmp_path, job_name)

  x <- new_bash(
    filename = script_path,
    job_name = job_name,
    tmp_path = tmp_path
    )

  # Adding options
  SBATCH$`job-name` <- NULL # Already added
  x$add_SBATCH(SBATCH)

  # Finalizing by setting the R call
  x$Rscript(file = file, flags = rscript_opt)

  # Writing the script
  x$write()

  message(
    "Sourcing an R script using Slurm. ",
    "The created file can be found here:\n ",
    script_path
    )

  #message("Here are the contents:", paste(readLines(script_path), collapse="\n"), "\n")
  sbatch(script_path, ...)
}
