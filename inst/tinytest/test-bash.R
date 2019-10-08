# context("Bash wrappers")
#
# test_that("If no slurm, then errors", {

if (!slurm_available()) {

  tmp <- tempdir()
  x <- suppressWarnings(Slurm_EvalQ(1+1, plan = "none", tmp_path = tmp))

  expect_error(scancel(1), "not available")
  expect_error(squeue(1), "not available")
  expect_error(sbatch(x), "not available")
  expect_error(sacct(1), "not available")
} else {

  expect_true(is.list(SchedulerParameters()))
  expect_true(length(slurm.conf()) > 0)

}
# })


