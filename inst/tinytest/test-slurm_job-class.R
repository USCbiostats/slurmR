if (slurm_available()) {

  # Making some space for the test
  tmpd <- tempfile(tmpdir=getwd())
  tmpf <- tempfile()

  job1  <- Slurm_EvalQ(1 + 1, njobs = 2, job_name = "test-Slurm_EvalQ",
             sbatch_opt = list(partition = "conti", account = "lc_dvc", time="02:00:00"),
             plan = "wait", tmp_path = tmpd
           )

  # I/O slurm job files
  write_slurm_job(job1, tmpf)
  job2 <- read_slurm_job(tmpf)
  ans  <- Slurm_collect(job1)

  # Checking the basics
  expect_equal(ans, list(2, 2))
  expect_equal(job1, job2)
  
  # Cleanup
  system2("rm", c("-rf", tmpd))

}


