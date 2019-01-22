#' The Slurm version of the `lapply`  function
#' @param X,FUN,mc.cores,... Arguments passed to [parallel::mclapply].
#' @param submit Logical, when `TRUE` calls [sbatch] to submit the job to slurm.
#' @template slurm
#' @param wait Logical scalar. When `TRUE` waits for the output to return.
#' (see [sbatch]).
#' @param njobs Integer. Number of jobs to specity.
#' @param sbatch_opt,rscript_opt List. Options to be passed via flags to
#' the bash file as `#SBATCH` and to `Rscript` respectively.
#' @param compress Logical scalar (default `TRUE`). Passed to [saveRDS]. Setting
#' this value to `FALSE` can be useful when the user requires faster read/write
#' of R objects on disk.
#' @references Job Array Support https://slurm.schedmd.com/job_array.html
#' @export
#' @examples
#' \dontrun{
#'   # A job drawing 1e6 uniforms on 10 jobs (array)
#'   # The option wait=TRUE makes it return only once the job is completed.
#'   job1 <- Slurm_lapply(1:20, function(i) runif(1e6), njobs=10, wait = TRUE)
#'
#'   # We can collect
#'   ans <- Slurm_collect(job1)
#'
#'   # Same as before, but not waiting this time, and we are passing more
#'   # arguments to the function
#'   job1 <- Slurm_lapply(1:20, function(i, a) runif(1e6, a), a = -1, njobs=10,
#'       wait = FALSE)
#'
#'   # We can submit
#'   job1 <- sbatch(job1)
#'
#'   # And cancel a job
#'   scancel(job1)
#'
#'   # And we can clean up
#'   Slurm_clean(job1)
#' }
Slurm_lapply <- function(
  X,
  FUN,
  ...,
  njobs    = 2L,
  mc.cores = getOption("mc.cores", 2L),
  job_name = opts_sluRm$get_job_name(),
  job_path = opts_sluRm$get_chdir(),
  submit   = TRUE,
  wait     = TRUE,
  sbatch_opt  = list(ntasks=1L, `cpus-per-task`=mc.cores),
  rscript_opt = list(vanilla=TRUE),
  compress = TRUE
  ) {

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

  # Checking function args
  FUNargs <- names(formals(FUN))
  dots    <- list(...)

  if (length(dots) && length(setdiff(names(dots), FUNargs)))
    stop("Some arguments passed via `...` are not part of `FUN`:\n -",
         paste(setdiff(names(dots), FUNargs), collapse="\n -"), call. = FALSE)


  # Setting the job name
  opts_sluRm$set_chdir(job_path)
  opts_sluRm$set_job_name(job_name)

  # Writing the data on the disk
  INDICES   <- parallel::splitIndices(length(X), njobs)
  dat       <- c(
    list(INDICES = INDICES, X = X, FUN = FUN, mc.cores=mc.cores),
    dots
    )
  obj_names <- save_objects(dat, compress = compress)

  # R Script -------------------------------------------------------------------

  # Initializing the script
  rscript <- new_rscript()

  # Adding readRDS
  rscript$add_rds(obj_names[-2])
  rscript$add_rds(obj_names[2], TRUE)

  # Adding actual code
  rscript$append(
    sprintf(
      "ans <- parallel::mclapply(\n%s\n)",
      paste(sprintf("    %-16s = .s%1$s", obj_names[-1]), collapse=",\n")
    )
  )

  # Finalizing and writing it out
  rscript$finalize(compress = compress)
  rscript$write()


  # Writing the bash script out ------------------------------------------------
  bash <- new_bash(njobs = njobs)

  if (!length(sbatch_opt) | (length(sbatch_opt) && !length(sbatch_opt$`cpus-per-task`)))
    sbatch_opt$`cpus-per-task` <- mc.cores
  if (!length(sbatch_opt) | (length(sbatch_opt) && !length(sbatch_opt$ntasks)))
    sbatch_opt$ntasks <- 1L

  bash$add_SBATCH(sbatch_opt)
  bash$append("export OMP_NUM_THREADS=1") # Otherwise mclapply may crash
  bash$finalize(rscript_opt)
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- new_slurm_job(
    call     = match.call(),
    rscript  = snames("r"),
    bashfile = snames("sh"),
    robjects = obj_names,
    njobs    = njobs,
    job_opts = opts_sluRm$get_opts()
  )

  if (submit)
    return(sbatch(ans, wait = wait))

  warning("The job has not been submitted yet. You can use `submit` to do so.",
          call. = FALSE)

  ans

}
