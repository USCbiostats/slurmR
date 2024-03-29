#' Submit an expression to be evaluated to multiple jobs.
#' @param expr An expression to be passed to Slurm.
#' @template slurm
#' @template job_name-tmp_path
#' @template sbatch_opt
#' @template rscript_opt
#' @template njobs
#' @return A list of length `njobs`.
#' @export
#' @importFrom stats setNames
Slurm_EvalQ <- function(
  expr,
  njobs       = 2L,
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
  sbatch_opt <- check_sbatch_opt(sbatch_opt, job_name = job_name, ntasks = 1L)

  # Setting the job name
  opts_slurmR$set_tmp_path(tmp_path)
  opts_slurmR$set_job_name(job_name)

  # Parsing expression ---------------------------------------------------------
  sexpr <- deparse(substitute(expr))

  # RSCRIPT --------------------------------------------------------------------
  if (is.null(export_env))
    export_env <- parent.frame()

  rscript <- new_rscript(
    njobs,
    libPaths = libPaths,
    tmp_path = tmp_path,
    job_name = job_name
    )

  if (length(export)) {
    rscript$add_rds(
      mget(export, envir = export_env), compress = compress, index = FALSE)
  }

  # Setting the seeds
  rscript$set_seed(seeds)
  rscript$append(
    paste0(
      "ans <- {list(\n",
      paste0(gsub("^", "   ", sexpr), collapse = "\n"),
      "\n)}"
      )
    )

  # Finalizing and writing it out
  rscript$finalize("ans", compress = compress)
  rscript$write()

  # BASH script ----------------------------------------------------------------
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

  # Processing hooks
  hooks <- c(hooks, list(function(res, job, ...) {
    stats::setNames(res, paste0(job$jobid, "_", 1:job$njobs))
  }))

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


