if (slurm_available()) {

  job <- Slurm_EvalQ(
    Sys.sleep(1000), njobs = 1L, plan = "submit",
    job_name = "test-wait_slurm1"
    )

  seconds <- system.time({
    wait_slurm(job, timeout = 5)
  })
  expect_true(difftime(system.time(), seconds, units = "secs") < 10)

  seconds <- system.time({
    wait_slurm(job$jobid, timeout = 5)
  })
  expect_true(difftime(system.time(), seconds, units = "secs") < 10)
  scancel(job)
  Slurm_clean(job)

}
