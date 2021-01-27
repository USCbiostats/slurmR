#' The Slurm version of the [`*apply`][lapply] family of functions.
#'
#' @param X,FUN,f,mc.cores,... Arguments passed to either [parallel::mclapply] or
#' [parallel::mcMap].
#' @template slurm
#' @template job_name-tmp_path
#' @template sbatch_opt
#' @template rscript_opt
#' @template njobs
#' @references Job Array Support https://slurm.schedmd.com/job_array.html
#' @export
#' @details The function `Slurm_lapply` will submit `njobs` to the queue and distribute
#' `X` according to [parallel::splitIndices]. For example, if `X` is list with
#' 1,000 elements, and `njobs = 2`, then `Slurm_lapply` will submit 2 jobs with
#' 500 elements of `X` each (2 chunks of data). The same principle applies to
#' `Slurm_sapply` and `Slurm_Map`, this is, the data is split by chunks so all
#' the information is sent at once when the job is submitted.
#'
#' @return If `plan == "collect"`, then whatever the analogous function returns,
#' otherwise, an object of class [slurm_job].
#'
#' @seealso For resubmitting a job, see the example in [sbatch].
#' @examples
#' \dontrun{
#'   # A job drawing 1e6 uniforms on 10 jobs (array)
#'   # The option plan = "wait" makes it return only once the job is completed.
#'   job1 <- Slurm_lapply(1:20, function(i) runif(1e6), njobs=10, plan = "wait")
#'
#'   # To collect
#'   ans <- Slurm_collect(job1)
#'
#'   # As before, but this time not waiting, and now we are passing more
#'   # arguments to the function
#'   # plan = "none" only creates the job object (and the files), we submit
#'   # later
#'   job1 <- Slurm_lapply(1:20, function(i, a) runif(1e6, a), a = -1, njobs=10,
#'       plan = "none")
#'
#'   # We submit
#'   job1 <- sbatch(job1)
#'
#'   # In order to cancel a job
#'   scancel(job1)
#'
#'   # How to clean up
#'   Slurm_clean(job1)
#' }
Slurm_lapply <- function(
  X,
  FUN,
  ...,
  njobs       = 2L,
  mc.cores    = 1L,
  job_name    = opts_slurmR$get_job_name(),
  tmp_path    = opts_slurmR$get_tmp_path(),
  plan        = "collect",
  sbatch_opt  = list(),
  rscript_opt = list(),
  seeds       = NULL,
  compress    = TRUE,
  export      = NULL,
  export_env  = NULL,
  libPaths    = .libPaths(),
  hooks       = NULL,
  overwrite   = TRUE,
  preamble    = NULL
  ) {

  # Figuring out what are we doing.
  plan <- the_plan(plan)

  # Checking the path
  check_full_path(
    tmp_path = tmp_path, job_name = job_name, overwrite = overwrite
    )

  # Checking job name
  sbatch_opt <- check_sbatch_opt(
    sbatch_opt,
    job_name        = job_name,
    `cpus-per-task` = mc.cores,
    ntasks          = 1L
    )

  # Checks
  if (!is.function(FUN))
    stop("FUN should be a function, instead it is: ", class(FUN), call. = FALSE)

  if (!is.list(X)) {
    warning(
      "`X` is not a list. The function will coerce it into one using `as.list`",
      call. = FALSE
      )
    X <- as.list(X)
  }

  # Checking the lengths
  if (length(X) < njobs) {
    warning("The number of jobs is greater than the length of `X`. The ",
            "`njobs`will be set equal to the length of `X`.", call. = FALSE,
            immediate. = TRUE)

    njobs <- length(X)
  }


  if (length(export) && !is.character(export))
    stop("`export` must be a character vector of object names.",
         call. = FALSE)

  # Checking function args
  FUNargs <- names(formals(FUN))
  dots    <- list(...)

  # All arguments in ... must be named
  dots_names <- names(dots)
  dots_names <- dots_names[dots_names!=""]
  if (length(dots_names) != length(dots))
    stop("One or more arguments in `...` are unnamed. All arguments passed ",
         "via `...` must be named.", call. = FALSE)

  if (length(dots) && length(setdiff(names(dots), FUNargs)))
    stop("Some arguments passed via `...` are not part of `FUN`:\n -",
         paste(setdiff(names(dots), FUNargs), collapse="\n -"), call. = FALSE)


  # Setting the job name
  opts_slurmR$set_job_name(job_name)
  opts_slurmR$set_tmp_path(tmp_path)

  # Splitting the components across the number of jobs. This will be used
  # later during the write of the RScript
  INDICES   <- parallel::splitIndices(length(X), njobs)

  # R Script -------------------------------------------------------------------

  # Initializing the script
  rscript <- new_rscript(
    njobs,
    libPaths = libPaths,
    tmp_path = tmp_path,
    job_name = job_name
    )

  # Adding readRDS
  if (is.null(export_env))
    export_env <- parent.frame()

  rscript$add_rds(list(INDICES = INDICES), split = FALSE, compress = FALSE)
  rscript$add_rds(list(X = X), split = TRUE, compress = compress)
  rscript$add_rds(
    c(
      list(FUN = FUN, mc.cores=mc.cores),
      dots,
      if (length(export))
        mget(export, envir = export_env)
      else
        NULL
    ), split = FALSE, compress = compress)

  # Setting the seeds
  rscript$set_seed(seeds)

  # Adding actual code
  rscript$append(
    sprintf(
      "ans <- parallel::mclapply(\n%s\n)",
      paste(
        sprintf("    %-16s = %1$s", setdiff(rscript$robjects[-1], export)),
        collapse=",\n"
        )
    )
  )

  # Finalizing and writing it out
  rscript$finalize("ans", compress = compress)
  rscript$write()


  # Writing the bash script out ------------------------------------------------
  bash <- new_bash(
    njobs    = njobs,
    job_name = job_name,
    output   = snames("out", job_name = job_name, tmp_path = tmp_path),
    filename = snames("sh", job_name = job_name, tmp_path = tmp_path)
    )

  bash$add_SBATCH(sbatch_opt)
  bash$append(c(opts_slurmR$get_preamble(), preamble))
  bash$Rscript(
    file  = snames("r", job_name = job_name, tmp_path = tmp_path),
    flags = rscript_opt
  )
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- new_slurm_job(
    call     = match.call(),
    rscript  = snames("r", job_name = job_name, tmp_path = tmp_path),
    bashfile = snames("sh", job_name = job_name, tmp_path = tmp_path),
    robjects = NULL,
    njobs    = njobs,
    opts_job = sbatch_opt,
    opts_r   = opts_slurmR$get_opts_r(),
    hooks    = hooks
  )

  if (plan$collect)
    return(Slurm_collect(sbatch(ans, wait = plan$wait, submit = plan$submit)))
  else
    return(sbatch(ans, wait = plan$wait, submit = plan$submit))

}
