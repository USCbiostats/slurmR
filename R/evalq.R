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
  sbatch_opt  = list(ntasks=1L),
  rscript_opt = list(vanilla=TRUE),
  seeds       = NULL,
  compress    = TRUE,
  export      = NULL,
  export_env  = NULL,
  libPaths    = .libPaths(),
  hooks       = NULL
) {

  # Figuring out what are we doing.
  plan <- the_plan(plan)

  # Setting the job name
  opts_slurmR$set_tmp_path(tmp_path)
  opts_slurmR$set_job_name(job_name)

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

  if (!length(sbatch_opt) | (length(sbatch_opt) && !length(sbatch_opt$ntasks)))
    sbatch_opt$ntasks <- 1L

  bash$add_SBATCH(sbatch_opt)
  # bash$append("export OMP_NUM_THREADS=1") # Otherwise mclapply may crash
  bash$Rscript(flags = rscript_opt)
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- new_slurm_job(
    call     = match.call(),
    rscript  = snames("r"),
    bashfile = snames("sh"),
    robjects = NULL,
    njobs    = njobs,
    opts_job = opts_slurmR$get_opts_job(),
    opts_r   = opts_slurmR$get_opts_r(),
    hooks    = hooks
  )

  if (plan$collect)
    return(Slurm_collect(sbatch(ans, wait = plan$wait, submit = plan$submit)))
  else
    return(sbatch(ans, wait = plan$wait, submit = plan$submit))

}
