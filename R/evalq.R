#' Submit an expression to be evaluated to multiple jobs
#' @param expr An expression to be passed to Slurm
#' @param collect Logical, when `TRUE`, if `submit = TRUE` and `wait = TRUE`,
#' the results are collected right away, otherwise, an object of class [slurm_job]
#' is returned.
#' @template slurm
#' @export
Slurm_EvalQ <- function(
  expr,
  njobs       = 2L,
  job_name    = opts_sluRm$get_job_name(),
  job_path    = opts_sluRm$get_chdir(),
  submit      = TRUE,
  wait        = TRUE,
  sbatch_opt  = list(ntasks=1L),
  rscript_opt = list(vanilla=TRUE),
  seeds       = 1L:njobs,
  compress    = TRUE,
  export      = NULL,
  collect     = TRUE
) {

  # Setting the job name
  opts_sluRm$set_chdir(job_path)
  opts_sluRm$set_job_name(job_name)

  # Parsing expression ---------------------------------------------------------
  sexpr <- deparse(substitute(expr))

  # RSCRIPT --------------------------------------------------------------------
  rscript <- new_rscript()

  if (length(export)) {
    rscript$add_rds(
      mget(export, envir=parent.frame()), compress = compress, index = FALSE)
  }

  # Setting the seeds
  rscript$set_seed(seeds)
  rscript$append(
    paste0(
      "ans <- list(tryCatch({\n",
      gsub("^", "   ", sexpr),
      "\n}, error = function(e) e))"
      )
    )

  # Finalizing and writing it out
  rscript$finalize("ans", compress = compress)
  rscript$write()

  # BASH script ----------------------------------------------------------------
  bash <- new_bash(njobs = njobs)

  if (!length(sbatch_opt) | (length(sbatch_opt) && !length(sbatch_opt$ntasks)))
    sbatch_opt$ntasks <- 1L

  bash$add_SBATCH(sbatch_opt)
  # bash$append("export OMP_NUM_THREADS=1") # Otherwise mclapply may crash
  bash$finalize(rscript_opt)
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- new_slurm_job(
    call     = match.call(),
    rscript  = snames("r"),
    bashfile = snames("sh"),
    robjects = NULL,
    njobs    = njobs,
    job_opts = opts_sluRm$get_opts()
  )

  if (collect && submit)
    Slurm_collect(sbatch(ans, wait = wait, submit = submit))
  else
    sbatch(ans, wait = wait, submit = submit)

}
