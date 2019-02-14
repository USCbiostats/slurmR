context("Lapply functions")

test_that("Works in debug mode", {

  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_chdir(x)
    opts_sluRm$set_job_name("lapply-1")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  b <- list(1:5, 1:10)

  ans <- suppressMessages(suppressWarnings(Slurm_lapply(b, mean, njobs=2, mc.cores = 1L)))
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], 3)

})


test_that("Exporting", {

  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_chdir(x)
    opts_sluRm$set_job_name("lapply-2")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  b      <- list(1:100, 4:10)
  mymean <- function(x) sum(x)/length(x)

  ans <- suppressMessages(suppressWarnings(
    Slurm_lapply(b, function(z) mymean(z), njobs=2, mc.cores = 1L, export="mymean")
    )
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], mean(b[[1]]))

})
