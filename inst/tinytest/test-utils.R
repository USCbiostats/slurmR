tmp <- file.path(opts_slurmR$get_tmp_path(), "test-utils")

# A new job directory is normalized before it exists. This must not emit the
# normalizePath "No such file or directory" warning.
new_path <- file.path(tempdir(), "slurmr-new-path", "job")
expect_silent(slurmR:::check_full_path(dirname(new_path), basename(new_path)))
expect_true(dir.exists(new_path))
unlink(file.path(tempdir(), "slurmr-new-path"), recursive = TRUE)

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
