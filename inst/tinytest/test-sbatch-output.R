if (.Platform$OS.type == "unix") local({
  path <- tempfile()
  dir.create(path)
  sbatch_path <- file.path(path, "sbatch")
  writeLines(c(
    "#!/bin/sh",
    "printf '%s\\n' 'Some informational message 999' \"$SLURMR_TEST_SBATCH_OUTPUT\""
  ), sbatch_path)
  Sys.chmod(sbatch_path, "0755")

  old_path <- Sys.getenv("PATH")
  old_output <- Sys.getenv("SLURMR_TEST_SBATCH_OUTPUT", unset = NA_character_)
  old_tmp_path <- opts_slurmR$get_tmp_path()
  old_job_name <- opts_slurmR$get_job_name()
  old_debug <- opts_slurmR$get_debug()
  old_verbose <- opts_slurmR$get_verbose()
  job_record <- slurmR:::LAST_SUBMITTED_JOB
  old_job <- last_job()
  on.exit({
    Sys.setenv(PATH = old_path)
    if (is.na(old_output))
      Sys.unsetenv("SLURMR_TEST_SBATCH_OUTPUT")
    else
      Sys.setenv(SLURMR_TEST_SBATCH_OUTPUT = old_output)
    opts_slurmR$set_tmp_path(old_tmp_path)
    opts_slurmR$set_job_name(old_job_name)
    suppressMessages(if (old_debug) opts_slurmR$debug_on() else opts_slurmR$debug_off())
    if (old_verbose) opts_slurmR$verbose_on() else opts_slurmR$verbose_off()
    job_record$job <- old_job
    unlink(path, recursive = TRUE)
  })

  Sys.setenv(PATH = paste(path, old_path, sep = .Platform$path.sep))
  suppressMessages(opts_slurmR$debug_off())

  outputs <- c(
    "Submitted batch job 17159340",
    "Submitted batch job 17159340 on cluster B",
    "Submitted batch job 17159340 on cluster cluster2"
  )

  for (i in seq_along(outputs)) {
    Sys.setenv(SLURMR_TEST_SBATCH_OUTPUT = outputs[i])
    job <- suppressMessages(Slurm_lapply(
      list(1, 2), identity, njobs = 2, plan = "submit",
      tmp_path = path, job_name = paste0("test-sbatch-output", i),
      sbatch_opt = list(cluster = "B")
    ))
    expect_equal(job$jobid, 17159340L)
    expect_equal(last_job()$jobid, 17159340L)
    expect_equal(read_slurm_job(dirname(job$bashfile))$jobid, 17159340L)
    expect_equal(suppressMessages(sbatch(job$bashfile)), 17159340L)
  }
})
