context("Bash wrappers")

test_that("If no slurm, then errors", {

  skip_if(slurm_available())

  tmp <- tempdir()
  x <- suppressWarnings(Slurm_EvalQ(1+1, submit = FALSE, job_path = tmp))

  expect_error(scancel(x), "not available")
  expect_error(squeue(x), "not available")
  expect_error(sbatch(x), "not available")
  expect_error(sacct(x), "not available")

})


