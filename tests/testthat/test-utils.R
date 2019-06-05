context("Utils")

test_that("Utility functions work", {

  tmp <- tempdir()
  x   <- suppressWarnings(
    Slurm_EvalQ(print("Hello"), plan = "none", job_path = tmp)
    )

  expect_true(dir.exists(paste0(tmp, "/", x$job_opts$`job-name`)))
  Slurm_clean(x)
  expect_true(!dir.exists(paste0(tmp, "/", x$job_opts$`job-name`)))

})
