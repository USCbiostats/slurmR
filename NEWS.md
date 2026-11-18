# slurmR 0.5-4

* Fixed job id recovery when submitting to another cluster (`sbatch -M`) or a
  federated cluster, where Slurm appends `on cluster <name>` to the success
  message (#41, #59). A clear error is now raised if the job id cannot be
  parsed.

* Valueless `#SBATCH` flags (e.g., `--exclusive`) are now handled by
  `read_sbatch()`, `sbatch()`, and `sourceSlurm()` (#44). **Breaking:**
  `read_sbatch()` now returns a list instead of a character vector.

* `read_sbatch()` now parses `#SBATCH` lines the way Slurm does: several
  options per line, trailing `# comments`, nested quotes and backslash escapes,
  empty values, and it stops at the first command. `parse_flags()` escapes
  quotes and backslashes in values (and quotes empty values and values with `#`).
  `sourceSlurm()` and `sbatch()` now honor `-J` as well as `--job-name`.

* Jobs submitted to another cluster remember it (new `cluster` field of
  `slurm_job` objects). `status()`, `wait_slurm()`, `sacct()`, `squeue()`, and
  `scancel()` now query that cluster (`--clusters`); `scancel()` used to wait
  forever for such jobs. `status()` and `wait_slurm()` gain the argument
  `clusters` for job ids. `squeue()` ignores the `CLUSTER:` header line.

* `wait_slurm(timeout = )` is now honored even when the job cannot be found
  (it used to wait forever when `force = TRUE`).

* `library()` calls in job scripts now respect `libPaths`, which fixes
  packages loaded from the `renv` cache (#43). Library paths are escaped in the
  generated script, so quotes and backslashes are safe.

* `Slurm_lapply()`, `Slurm_Map()`, and `Slurm_EvalQ()` now error early when
  `export` contains names reserved by the job script (#49). Also fixed
  `Slurm_EvalQ(export = ...)`, which failed with "unused argument".

* `sacct()`/`status()`/`wait_slurm()` keep the `slurm_job` object when falling
  back from `sacct` (#40).

* Adressing [`roxygen2` issue #1491](https://github.com/r-lib/roxygen2/issues/1491).


# slurmR 0.5-3

*  Fixed bug in command line tool `slurmr` that overwrites source file when
   located in the same folder.

# slurmR 0.5-2

*  New integer method for `get_tmp_path`, `get_job_name`, and `get_job_id`. This
   is a fix for issue #29 (first reported by @thistleknot.)


# slurmR 0.5-1 (CRAN)

*  `sourceSlurm(plan = "collect")` won't wait forever.

*  More verbose error messages when `Slurm_collect` returns with errors.

*  The function `sacct()` now doesn't fail when accounting is not available.

*  slurmR now has a Dockerimage that can be used to test with a Slurm cluster.
   The docker image can be started from within R (if Docker is available).

*  `makeSlurmCluster()`s default tmp_path is now `opts_slurmR$get_tmp_path()`.

*  New functions `slurmr_docker_*` provide a wrapper to interact with the
   Dockerimage of slurmR.


# slurmR 0.4-3 (CRAN)

Minor release.

## New features

*  Added a `preamble` option for Slurm batch scripts. This allows the user to
   specify commands that need to be added to the script, e.g., `module load`.
  
*  `preamble` can be specified via `opts_slurmR$set_preamble()` or directly
   when calling `Slurm_*apply`.
  
*  Added the function `opts_slurmR$reset()`.
  
## Bug fixes

*  `sourceSlurm()` was using a file created at `tempdir()` which was deleted,
   and thus, unavailable to be used by `sbatch`. Tempfiles like those are now
   created at `dirname(tempdir())`.

# slurmR 0.4-2

Minor release.

## Bug fixes

*  Fixes a user-message issue (not crucial) observed in Fedora and Solaris.

# slurmR 0.4-1 (CRAN)

## Bug fixes

*  Fixed bug when collecting failed jobs. slurmR would fail to
   correctly list failed jobs and thus to collect outputs. This would also
   would affect when trying to collect partially resubmitted jobs.
  
*  Slurm options passed via `opts_slurmR` are now passed to
   `Slurm_*apply` as documented. Thanks to Gregory Penn (gregorypenn) who
   reported the bug.
  
*  `slurmr_cmd` now expands the `cmd_path`.

## Misc
  
*  Increased code coverage.

*  The function `wait_slurm()` is now exported and documented.

*  Each line in the written R script is now wrapped with a custom
   `tryCatch` function (`tcq`). On error, this new function will collect both
   the expression called, the error message, and will quit the R session, so now
   the user has a better description of possible errors, e.g. missing
   packages.

# slurmR 0.3-0

*  Name changed from `sluRm` to `slurmR`.

# slurmR 0.2-0

*  JOSS review (finalizing).

# slurmR 0.1-0

*  Added a `NEWS.md` file to track changes to the package.

