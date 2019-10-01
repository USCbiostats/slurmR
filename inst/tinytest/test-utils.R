# context("Utils")
#
# test_that("Utility functions work", {

  tmp <- getwd()
  x   <- suppressWarnings(
    Slurm_EvalQ(print("Hello"), plan = "none", tmp_path = tmp)
    )

  expect_true(dir.exists(paste0(tmp, "/", x$opts_job$`job-name`)))
  Slurm_clean(x)
  expect_true(!dir.exists(paste0(tmp, "/", x$opts_job$`job-name`)))

# })
