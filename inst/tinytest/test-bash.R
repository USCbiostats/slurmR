if (!slurm_available()) {

  x <- suppressWarnings(Slurm_EvalQ(1+1, plan = "none"))

  expect_error(scancel(1), "not available")
  expect_error(squeue(1), "not available")
  expect_error(sbatch(x), "not available")
  expect_error(sacct(1), "not available")

} else {

  expect_true(is.list(SchedulerParameters()))
  expect_true(length(slurm.conf()) > 0)

  expect_error(sbatch("unexisting.slurm"))

  ans1 <- Slurm_EvalQ(slurmR::WhoAmI(), njobs = 2, plan = "wait",
    job_name = "test-Slurm_EvalQ1"
  )
  ans1_cpy <- last_job()

  opts_slurmR$verbose_on()
  #expect_warning(
    Slurm_EvalQ(slurmR::WhoAmI(), njobs = 2, plan = "submit",
      job_name = "test-Slurm_EvalQ2")
  #)
  expect_true(inherits(sacct(ans1), "data.frame"))
  opts_slurmR$verbose_off()


  expect_equal(ans1_cpy, ans1)
  expect_true(inherits(squeue(ans1), "data.frame"))
  expect_true(inherits(squeue(ans1$jobid), "data.frame"))

  expect_message(Slurm_log(ans1))
 
  Slurm_clean(ans1)

}



