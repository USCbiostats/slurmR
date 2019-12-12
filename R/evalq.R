#' Submit an expression to be evaluated to multiple jobs.
#' @param expr An expression to be passed to Slurm.
#' @template slurm
#' @template job_name-tmp_path
#' @template sbatch_opt
#' @template rscript_opt
#' @template njobs
#' @return A list of length `njobs`.
#' @export
Slurm_EvalQ <- function(
  expr,
  njobs       = 2L,
  job_name    = random_job_name(),
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
  overwrite   = TRUE
) {

  # Figuring out what are we doing.
  plan <- the_plan(plan)

  # Checking job name
  sbatch_opt <- check_sbatch_opt(sbatch_opt, job_name = job_name, ntasks = 1L)

  # Setting the job name
  opts_slurmR$set_tmp_path(tmp_path, overwrite = overwrite)
  opts_slurmR$set_job_name(job_name, overwrite = overwrite)

  # Parsing expression ---------------------------------------------------------
  sexpr <- deparse(substitute(expr))

  # RSCRIPT --------------------------------------------------------------------
  if (is.null(export_env))
    export_env <- parent.frame()

  rscript <- new_rscript(njobs, libPaths = libPaths)

  if (length(export)) {
    rscript$add_rds(
      mget(export, envir = export_env), compress = compress, index = FALSE)
  }

  # Setting the seeds
  rscript$set_seed(seeds)
  rscript$append(
    paste0(
      "ans <- list(tryCatch({\n",
      paste0(gsub("^", "   ", sexpr), collapse = "\n"),
      "\n}, error = function(e) e))"
      )
    )

  # Finalizing and writing it out
  rscript$finalize("ans", compress = compress)
  rscript$write()

  # BASH script ----------------------------------------------------------------
  bash <- new_bash(
    njobs    = njobs,
    job_name = opts_slurmR$get_job_name(),
    output   = snames("out"),
    filename = snames("sh")
    )

  bash$add_SBATCH(sbatch_opt)
  bash$Rscript(flags = rscript_opt)
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- new_slurm_job(
    call     = match.call(),
    rscript  = snames("r"),
    bashfile = snames("sh"),
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
