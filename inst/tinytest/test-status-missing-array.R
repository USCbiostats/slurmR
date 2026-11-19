# Some clusters omit cancelled, never-started array tasks from sacct. Verify
# that status() retains the submitted selection, fills current tasks from
# squeue, and marks tasks absent from both commands as failed.
if (.Platform$OS.type == "unix") local({

  fake_bin <- tempfile("slurmr-missing-array-")
  dir.create(fake_bin)
  old_path <- Sys.getenv("PATH")
  on.exit({
    Sys.setenv(PATH = old_path)
    unlink(fake_bin, recursive = TRUE)
  })

  commands <- list(
    sbatch = c("#!/bin/sh", "echo 'Submitted batch job 999'"),
    sacct = c(
      "#!/bin/sh",
      "printf 'JobID|State|ExitCode|\\n999_1|COMPLETED|0:0|\\n'"
    ),
    squeue = c(
      "#!/bin/sh",
      "printf 'JOBID|STATE|\\n999_2|PENDING|\\n'"
    )
  )
  for (cmd in names(commands)) {
    path <- file.path(fake_bin, cmd)
    writeLines(commands[[cmd]], path)
    Sys.chmod(path, "0755")
  }
  Sys.setenv(PATH = paste(fake_bin, old_path, sep = .Platform$path.sep))

  job <- suppressWarnings(
    Slurm_lapply(
      as.list(1:3), identity, njobs = 3L, plan = "none", tmp_path = tempdir(),
      job_name = "test-status-missing-array", overwrite = TRUE
    )
  )
  suppressMessages(sbatch(job, array = "1-3"))
  expect_equal(job$jobid, 999L)
  expect_equal(job$array_ids, 1:3)

  s <- status(job)
  expect_equal(as.integer(s), 99L)
  expect_equal(attr(s, "njobs"), 3L)
  expect_equal(s$done, 1L)
  expect_equal(s$pending, 2L)
  expect_equal(s$failed, 3L)

  answer1 <- slurmR:::snames(
    "rds", 1L, tmp_path = job$opts_r$tmp_path,
    job_name = job$opts_job$`job-name`
  )
  saveRDS(list(1L), answer1)
  expect_error(
    Slurm_collect(job),
    "task ID\\(s\\): 2, 3.*03-answer-002\\.rds.*03-answer-003\\.rds"
  )

  unlink(file.path(job$opts_r$tmp_path, job$opts_job$`job-name`), recursive = TRUE)

})
