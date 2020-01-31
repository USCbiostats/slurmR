verify_lengths <- function(env, nam) {

  # Is there anything we should be doing?
  if (length(env[[nam]]) == 1)
    return(invisible())

  # Yes, we need to work on it
  idx <- seq_along(env[[nam]])
  idx <- idx[order(-sapply(env[[nam]], length))]

  # First default length
  curlength <- length(env[[nam]][[idx[1]]])

  for (i in idx[-1]) {

    len <- length(env[[nam]][[i]])
    if (len == 1) {

      # If it is atomic
      if (is.atomic(env[[nam]][[i]]))
        env[[nam]][[i]] <- rep_len(env[[nam]][[i]], curlength)
      else
        env[[nam]][[i]] <- replicate(curlength, env[[nam]][[i]], simplify=FALSE)

    } else if (len != curlength)
      stop("Arguments passed via `...` differ in lengths.", call. = FALSE)

  }

  invisible(NULL)

}

#' @export
#' @rdname Slurm_lapply
Slurm_Map <- function(
  f,
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
  overwrite   = TRUE
  ) {

  # Figuring out the plan
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
  f <- match.fun(f)
  if (!is.function(f))
    stop("f should be a function, instead it is: ", class(f), call. = FALSE)

  if (length(export) && !is.character(export))
    stop("`export` must be a character vector of object names.",
         call. = FALSE)

  # Checking function args
  FUNargs <- names(formals(f))
  dots    <- list(...)

  # Checking names
  # All arguments in ... must be named
  dots_names <- names(dots)
  dots_names <- dots_names[dots_names!=""]
  if (length(dots_names) != length(dots))
    stop("One or more arguments in `...` are unnamed. All arguments passed ",
         "via `...` must be named.", call. = FALSE)

  if (length(dots) && length(setdiff(names(dots), FUNargs)))
    stop("Some arguments passed via `...` are not part of `f`:\n -",
         paste(setdiff(names(dots), FUNargs), collapse="\n -"), call. = FALSE)

  # Verify lengths and recycle
  verify_lengths(environment(), "dots")

  # Checking the lengths
  if (length(dots[[1]]) < njobs) {
    warning("The number of jobs is greater than the length of `",
            names(dots)[1], "`. The variable `njobs` will be set equal to the ",
            "length of it.", call. = FALSE,
            immediate. = TRUE)

    njobs <- length(dots[[1]])
  }

  # Setting the job name
  opts_slurmR$set_tmp_path(tmp_path)
  opts_slurmR$set_job_name(job_name)

  # Writing the data on the disk -----------------------------------------------
  INDICES   <- parallel::splitIndices(length(dots[[1]]), njobs)

  # R Script -------------------------------------------------------------------

  # Initializing the script
  rscript <- new_rscript(
    njobs,
    libPaths = libPaths,
    tmp_path = tmp_path,
    job_name = job_name
    )

  # Adding readRDS
  rscript$add_rds(list(INDICES = INDICES), split = FALSE, compress = FALSE)
  rscript$add_rds(list(f = f, mc.cores=mc.cores), split = FALSE, compress = compress)
  rscript$add_rds(dots, split = TRUE, compress = compress)

  # Adding readRDS
  if (is.null(export_env))
    export_env <- parent.frame()

  if (length(export))
    rscript$add_rds(mget(export, envir = export_env), split = FALSE, compress = compress)

  # Setting the seeds
  rscript$set_seed(seeds)

  # Adding actual code
  rscript$append(
    sprintf(
      "ans <- parallel::mcMap(\n%s\n)",
      paste(
        sprintf(
          "    %-16s = %1$s",
          setdiff(c(rscript$robjects[-1]), export)
          ),
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
    job_name = opts_slurmR$get_job_name(),
    output   = snames("out", tmp_path = tmp_path, job_name = job_name),
    filename = snames("sh", tmp_path = tmp_path, job_name = job_name)
    )

  bash$add_SBATCH(sbatch_opt)
  bash$append("export OMP_NUM_THREADS=1") # Otherwise mclapply may crash
  bash$Rscript(
    file  = snames("r", job_name = job_name, tmp_path = tmp_path),
    flags = rscript_opt
  )
  bash$write()

  # Returning ------------------------------------------------------------------
  ans <- new_slurm_job(
    call     = match.call(),
    rscript  = snames("r", tmp_path = tmp_path, job_name = job_name),
    bashfile = snames("sh", tmp_path = tmp_path, job_name = job_name),
    robjects = NULL,
    njobs    = njobs,
    opts_job = sbatch_opt,
    opts_r   = opts_slurmR$get_opts_r(),
    hooks    = hooks
  )

  if (plan$collect)
    return(Slurm_collect(sbatch(ans, wait = plan$submit, submit = plan$submit)))
  else
    return(sbatch(ans, wait = plan$submit, submit = plan$submit))

}

