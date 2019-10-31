#' Source an R script as a Slurm job
#'
#' This function sources R scripts using Slurm by creating a batch script file
#' and submitting it via [sbatch].
#'
#' @param file Character. Path to the R script to source using Slurm.
#' @template job_name-tmp_path
#' @template rscript_opt
#' @param ... Further options passed to [sbatch].
#' @param plan A character scalar. (See [the_plan]).
#'
#' @details
#' `sourceSlurm` checks for flags that may be included in the Slurm job file. If
#' the R script starts with `#!/bin/` or similar, then `#SBATCH` flags will be
#' read from the R script and added to the Slurm job file.
#'
#' @return In the case of `sourceSlurm`, Whatever [sbatch] returns.
#' @export
#' @examples
#' # In this example we will be sourcing an R script that also has #SBATCH
#' # flags. Here are the contents
#' file <- system.file("example.R", package="slurmR")
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
#' # #SBATCH --output=/home/vegayon/Documents/slurmR/Waiting.out
#' # #SBATCH --account=lc_ggv
#' # #SBATCH --time=01:00:00
#' # #SBATCH --mem-per-cpu=4G
#' # /usr/lib/R/bin/Rscript --vanilla /usr/local/lib/R/site-library/slurmR/example.R
sourceSlurm <- function(
  file,
  job_name    = NULL,
  tmp_path    = tempdir(),
  rscript_opt = list(vanilla = TRUE),
  plan        = "submit",
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
    job_name = job_name
    )

  # Adding options
  SBATCH$`job-name` <- NULL # Already added
  x$add_SBATCH(SBATCH)

  # Finalizing by setting the R call
  x$Rscript(file = file, flags = rscript_opt)

  # Writing the script
  x$write()

  if (interactive())
    message(
      "\nSourcing an R script using Slurm. ",
      "The created file can be found here:\n ",
      script_path
      )
  else {
    message("\n") # We need an extra skip
    hline(
      "Sourcing an R script using Slurm.",
      "The bashscript has the following contents:"
    )
    message(paste(readLines(script_path), collapse="\n"))
    hline("EOF")
  }

  # Figuring out the plan
  plan <- the_plan(plan)
  if (plan$collect)
    warning("When using Slurm via sourceSlurm, collection is not possible.", call. = FALSE)

  # Submitting the job
  sbatch(script_path, submit = plan$submit, wait = plan$wait, ...)

}

#' @export
#' @rdname sourceSlurm
#' @param cmd_path Character scalar. Path (directory) where to put the comand function.
#' This is usually your home directory.
#' @param cmd_name Character scalar. Name of the command (of the file).
#' @param add_alias,bashrc_path Logical scalar and character scalar. When
#' `add_alias=TRUE` it will modify (or create, if non-existent) the `.bashrc`
#' file to add an alias of the same name of `cmd_name`. The path to `.bashrc` can be
#' specified via the `bashrc_path` option.
#' @details The function `slurmr_cmd` writes a simple command that works as a wrapper
#' of `sourceSlurm`. In particular, from command line, if the user wants to source an
#' R script using `sourceSlurm`, we can either:
#'
#' ```
#' $ Rscript -e "slurmR::sourceSlurm('path/to/the/script.R', plan = 'submit')"
#' ```
#'
#' Or, after calling `slurmr_cmd` from within R, do the following instead
#'
#' ```
#' $ ./sbatchr path/to/the/script.R
#' ```
#'
#' And, if you used the option `add_alias = TRUE`, then, after restarting bash,
#' you can run R scripts with Slurm as follows:
#'
#' ```
#' $ sbatchr path/to/the/script.R
#' ```
#'
#' The main side effect of this function is that it creates a file named `cmd_name`
#' in the directory specified by `cmd_path`, and, if `add_alias = TRUE`. it will
#' create (if not found) or modify (if found) the `.bashrc` file adding a line
#' with an alias. For more information on `.bashrc` see [here](https://superuser.com/questions/49289).
#'
#' @return The function `slurmr_cmd` returns `invisible()`.
#' @rdname sourceSlurm
slurmr_cmd <- function(cmd_path, cmd_name = "sbatchr", add_alias = TRUE, bashrc_path = "~/.bashrc") {

  fn   <- suppressWarnings(normalizePath(sprintf("%s/%s", cmd_path, cmd_name), ))
  bash <- new_bash(fn)
  bash$Rscript("", flags = list(vanilla = TRUE, e = "slurmR::sourceSlurm('$1', plan = 'submit')"))
  bash$write()

  system2("chmod", sprintf("u+x %s", fn))

  if (add_alias) {

    # This is the entry
    l_alias <- sprintf(
      "# Alias created by the slurmR R package on %1$s\nalias %2$s=\"%3$s/./%2$s\"",
      as.character(Sys.time()),
      cmd_name,
      cmd_path
    )

    # Checking whether the file exists or not
    if (file.exists(bashrc_path)) {

      # Reading the lines and setting the exit protocol in case of emergency
      f_bashrc <- readLines(bashrc_path)
      fn_bashrc_tmp <- tempfile()
      writeLines(text = f_bashrc, con = fn_bashrc_tmp)
      on.exit({
        if (!exists("all_ok")) {
          file.copy(from = fn_bashrc_tmp, to = bashrc_path, overwrite = TRUE)
          message(
            "Something went wrong during the update of the .bashrc file. ",
            "The original file has been restored."
            )
        }
      })

      # Does it exists already?
      loc <- which(grepl(paste0("^alias ", cmd_name), f_bashrc))
      if (length(loc)) f_bashrc[loc[1]] <- l_alias
      else f_bashrc <- c(f_bashrc, l_alias)

    } else
      f_bashrc <- l_alias

    # Re-writing the file
    writeLines(text = f_bashrc, con = bashrc_path)

  }

  message(
    "Success! The file has been written in: \n  ", fn,
    ifelse(
      add_alias,
      paste0(
        "\nand an alias has been added to:\n",
        bashrc_path,
        "\nYou can start using it in command line after sourcing it with ",
        "`source ", bashrc_path,"` or restarting bash."
        ),
      ""
      ),
    "\nYou can submits jobs from your command line using the following:\n",
    ifelse(add_alias, "  ", "  ./"), cmd_name, " path/to/rscript/to/run.R\n",
    "Remember that this is a wrapper of `sourceSlurm`, to the file must start with '#!/bin/sh'."
    )

  all_ok <- TRUE
  return(invisible())

}

