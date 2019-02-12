context("Lapply functions")

test_that("Works in debug mode", {

  # Tempdirectory
  x <- tempdir()

  opts_sluRm$set_chdir(x)
  opts_sluRm$set_job_name("testthat")
  opts_sluRm$debug_on()

  b <- list(1:5, 1:10)

  ans <- suppressMessages(suppressWarnings(Slurm_lapply(b, mean, njobs=2, mc.cores = 1L)))
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], 3)

})
