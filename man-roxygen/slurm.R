#' @param plan A character scalar. (See [the_plan]).
#' @param compress Logical scalar (default `TRUE`). Passed to [saveRDS]. Setting
#' this value to `FALSE` can be useful when the user requires faster read/write
#' of R objects on disk.
#' @param seeds Integer vector of length `njobs`. Seeds to be passed to each
#' job. When `NULL` (default), seeds will be picked automatically (see [new_rscript]).
#' @param export A character vector with the names of the objects to be
#' included in the spawned sessions (looked up in `export_env`). Names that
#' slurmR uses internally in the job's R script are reserved and cannot be
#' exported: `Slurm_env`, `ARRAY_ID`, `snames`, `TMP_PATH`, `JOB_NAME`, `tcq`,
#' and `seeds`. In `Slurm_lapply` and `Slurm_sapply`, `INDICES`, `X`, `FUN`,
#' and `mc.cores` are also reserved; in `Slurm_Map`, `INDICES`, `f`, and
#' `mc.cores` are. Names of arguments passed via `...` cannot be exported
#' either. The function returns with an error before writing anything to disk
#' if any of the exported names clash with these.
#' @param export_env An environment. Environment where the objects listed in
#' `export` are located (default [parent.frame()]).
#' @param libPaths A character vector. See [.libPaths].
#' @param hooks A list of functions (passed to [new_slurm_job]).
#' @param overwrite Logical scalar. When `TRUE`, if the path specified by
#' `tmp_path/job_name` already exists, it will overwrite it, otherwise the
#' function returns with an error.
#' @param preamble Character vector. Each element is then added to the Slurm
#' batch file between the `#SBATCH` options and the script's main call. A
#' common example is adding required modules, e.g. `c("module load gcc/6.1.1")`.
#'
NULL
