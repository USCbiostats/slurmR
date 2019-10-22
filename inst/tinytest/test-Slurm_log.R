
if (slurm_available()) {

  # Setting up the options
  opts_slurmR$set_opts(partition="scavenge")
  opts_slurmR$debug_off()

  x <- Slurm_EvalQ(1+1, njobs = 2, job_name = "test-Slurm_log1", plan = "none")
  expect_true(status(x) == -1L)
  sbatch(x, wait = TRUE)

  # Checking errors
  expect_error(Slurm_log(x, which. = c(1,2)), "length 1")
  expect_error(Slurm_log(x, which. = 3), "within")
  # expect_error(Slurm_log(x, cmd = "acommandthatdoesntexists"), "system admin")
  Slurm_clean(x)
  expect_error(Slurm_log(x), "path to its temp")

}
