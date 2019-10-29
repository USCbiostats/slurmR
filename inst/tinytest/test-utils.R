tmp <- ifelse(slurm_available(), "/staging/ggv/", tempdir())
x   <- suppressWarnings(
  Slurm_EvalQ(print("Hello"), plan = "none", tmp_path = tmp)
  )

expect_true(dir.exists(paste0(tmp, "/", x$opts_job$`job-name`)))
Slurm_clean(x)
expect_true(!dir.exists(paste0(tmp, "/", x$opts_job$`job-name`)))

if (slurm_available()) {

  x <- suppressWarnings({
    Slurm_EvalQ(
      slurmR::whoami(),
      njobs      = 2,
      sbatch_opt = list(partition="scavenge"),
      plan       = "wait",
      job_name   = "test-whoami"
    )
  })

  expect_true(is.character(unlist(Slurm_collect(x))))
  expect_silent(status(x))
  expect_true(is.vector(status(x)$done))
  expect_equal(Slurm_env("SLURM_ARRAY_TASK_ID"), 1)

}
