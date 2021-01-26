# tmp <- ifelse(slurm_available(), "/staging/ggv/", tempdir())
x   <- suppressWarnings(
  Slurm_EvalQ(print("Hello"), plan = "none", tmp_path = tmp)
  )
expect_true(is.character(whoami()))
expect_true(dir.exists(paste0(tmp, "/", x$opts_job$`job-name`)))
Slurm_clean(x)
expect_true(!dir.exists(paste0(tmp, "/", x$opts_job$`job-name`)))

if (slurm_available()) {

  x <- suppressWarnings({
    Slurm_EvalQ(
      slurmR::whoami(),
      njobs      = 2,
      plan       = "wait",
      job_name   = "test-utils1"
    )
  })

  expect_true(is.character(unlist(Slurm_collect(x))))
  expect_silent(status(x))
  Sys.sleep(2) # Just in case we need to wait a bit
  expect_true(is.vector(status(x)$done))
  Slurm_clean(x)
  expect_equal(Slurm_env("SLURM_ARRAY_TASK_ID"), 1)

}
