# Integration test for issue #40 (related to #29): clusters without Slurm
# accounting. On those, -sacct- fails and status() falls back to checking the
# output files of the job. That fallback needs the full slurm_job object, which
# used to be dropped by status.slurm_job() and wait_slurm.slurm_job() (they
# only passed the job ID down), resulting in
# "Error in x$njobs : $ operator is invalid for atomic vectors".
#
# Since we cannot turn accounting off on a real cluster, we simulate it with
# fake `sbatch`, `sacct`, and `squeue` executables that always fail. With
# these, slurm_available() returns TRUE but every call to -sacct- (and
# -squeue-) fails, just like on a cluster without accounting. This test only
# runs when Slurm is not installed, so the fake executables do not shadow a
# real Slurm installation.
if ((.Platform$OS.type == "unix") && !slurm_available()) {

  # Creating the fake Slurm binaries --------------------------------------------
  old_path <- Sys.getenv("PATH")
  fake_bin <- tempfile("slurmr-fake-bin-")
  dir.create(fake_bin)
  for (cmd in c("sbatch", "sacct", "squeue")) {
    f <- file.path(fake_bin, cmd)
    writeLines(c("#!/bin/sh", "echo 'accounting disabled' >&2", "exit 1"), f)
    Sys.chmod(f, mode = "0755")
  }
  Sys.setenv(PATH = paste(fake_bin, old_path, sep = .Platform$path.sep))

  expect_true(slurm_available())

  # Evaluates an expression, collecting (and muffling) the warnings
  with_warnings <- function(expr) {
    warns <- character(0)
    value <- withCallingHandlers(
      expr,
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warns)
  }

  # Creating the job (not submitted) and faking a submission ------------------
  job <- Slurm_lapply(
    1:4,
    function(i) i,
    njobs    = 2L,
    plan     = "none",
    tmp_path = tempdir(),
    job_name = "test-sacct_fallback"
  )

  job$jobid <- 999999L

  # The call to sacct must fail, which triggers the fallback
  expect_error(sacct(job), "error has occurred")

  # Mimicking completed array tasks: output and answer files ------------------
  tmp_path <- job$opts_r$tmp_path
  job_name <- job$opts_job$`job-name`
  rds_files <- slurmR:::snames(
    "rds",
    1:2,
    tmp_path = tmp_path,
    job_name = job_name
  )
  saveRDS(list(1L, 2L), rds_files[1])
  saveRDS(list(3L, 4L), rds_files[2])

  out_files <- sprintf(
    gsub(
      "%(A|a)",
      "%s",
      slurmR:::snames("out", tmp_path = tmp_path, job_name = job_name)
    ),
    999999L,
    1:2
  )
  file.create(out_files)

  # status() on the slurm_job object ------------------------------------------
  s <- with_warnings(status(job))
  expect_true(any(grepl("sacct- failed", s$warnings)))
  s <- s$value
  expect_equal(as.integer(s), 0L)
  expect_equal(attr(s, "njobs"), 2L)
  expect_equal(sort(s$done), 1:2)
  expect_equal(length(s$failed), 0L)

  # wait_slurm() on the slurm_job object --------------------------------------
  time0 <- Sys.time()
  ans   <- with_warnings(wait_slurm(job, timeout = 30, freq = 0.01))
  expect_null(ans$value)
  expect_true(difftime(Sys.time(), time0, units = "secs") < 30)
  expect_false(any(grepl("Timeout", ans$warnings)))
  expect_true(any(grepl("sacct- failed", ans$warnings)))

  # Collecting (what Slurm_lapply(..., plan = "collect") does) -----------------
  ans <- suppressWarnings(Slurm_collect(job))
  expect_equal(unname(unlist(ans)), 1:4)

  # Partially failed job: task 2 has no answer and is not in the queue --------
  file.remove(rds_files[2])
  s <- suppressWarnings(status(job))
  expect_equal(as.integer(s), 99L)
  expect_equal(s$done, 1L)
  expect_equal(s$failed, 2L)

  # wait_slurm() also returns, since all tasks are either done or failed
  time0 <- Sys.time()
  suppressWarnings(wait_slurm(job, timeout = 30, freq = 0.01))
  expect_true(difftime(Sys.time(), time0, units = "secs") < 30)

  # Bare job IDs ---------------------------------------------------------------
  # If the ID matches the last submitted job, the fallback recovers the object.
  saveRDS(list(3L, 4L), rds_files[2])
  slurmR:::LAST_SUBMITTED_JOB$set(job)
  s <- suppressWarnings(status(999999L))
  expect_equal(as.integer(s), 0L)
  expect_equal(sort(s$done), 1:2)

  # Otherwise, a clear error message instead of the `$` crash.
  expect_error(
    suppressWarnings(status(123456L)),
    "requires an object of class `slurm_job`"
  )

  # Cleaning up ----------------------------------------------------------------
  Sys.setenv(PATH = old_path)
  unlink(fake_bin, recursive = TRUE)
  unlink(file.path(tmp_path, job_name), recursive = TRUE)

}
