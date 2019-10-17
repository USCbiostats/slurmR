if (slurm_available()) {

  # Making some space for the test
  opts_slurmR$set_tmp_path("/staging/ggv")
  opts_slurmR$set_opts(partition="scavenge")

  tmpf <- tempfile()

  job1  <- Slurm_EvalQ(
    1 + 1, njobs = 2, job_name = "test-slurm_job-class1",
    plan = "wait"
    )

  # I/O slurm job files
  write_slurm_job(job1, tmpf)
  job2 <- read_slurm_job(tmpf)
  ans  <- Slurm_collect(job1)

  # Checking the basics
  expect_equal(ans, list(2, 2))
  expect_equal(job1, job2)

  expect_error(
    Slurm_EvalQ(1+1, njobs=2, job_name="test-slurm_job-class2", hooks = letters),
    "should be functions"
  )

  # Cleanup
  system2("rm", c("-rf", tmpd))
  Slurm_clean(job1)
  Slurm_clean(job2)
  Slurm_clean(job3)

}


