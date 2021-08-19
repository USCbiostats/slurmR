if (slurm_available()) {

  tmpf <- tempfile()

  job1  <- Slurm_EvalQ(
    1 + 1, njobs = 2, job_name = "test-slurm_job-class1",
    plan = "wait"
    )

  # I/O slurm job files
  write_slurm_job(job1, tmpf)
  job2 <- read_slurm_job(tmpf)
  job3 <- read_slurm_job(write_slurm_job(job1))
  ans  <- Slurm_collect(job1)

  # Checking the basics
  expect_equal(unname(ans), list(2, 2))

  job1 <- mget(sort(ls(envir = job1)), envir = job1)
  job2 <- mget(sort(ls(envir = job2)), envir = job2)
  job3 <- mget(sort(ls(envir = job3)), envir = job3)

  # Removing hooks
  job1 <- job1[names(job1) != "hooks"]
  job2 <- job2[names(job2) != "hooks"] 
  job3 <- job3[names(job3) != "hooks"]

  expect_equal(job1, job2)
  expect_equal(job1, job3)

  expect_error(
    Slurm_EvalQ(1+1, njobs=2, job_name="test-slurm_job-class2", hooks = letters),
    "should be functions"
  )

  expect_error(read_slurm_job(paste0(
    opts_slurmR$get_tmp_path(), "/test-slurm_job-class1/jobs1.rds"
  )))
  expect_error(read_slurm_job(paste0(
    opts_slurmR$get_tmp_path(), "/"
  )))

  # Cleanup

  # Slurm_clean(job1)
  # Slurm_clean(job2)



}


