context("Map functions")

test_that("Works in debug mode", {

  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_chdir(x)
    opts_sluRm$set_job_name("map-1")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  b <- list(1:5, 1:10)
  a <- list(USArrests, USAccDeaths)

  ans <- suppressMessages(suppressWarnings(
    Slurm_Map(function(x,y) list(x=x, y=y), njobs=2, mc.cores = 1L, x=a, y=b))
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], list(x=a[[1]], y=b[[1]]))

})


test_that("Exporting", {

  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_chdir(x)
    opts_sluRm$set_job_name("map-2")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  b <- list(1:5, 1:10)
  a <- list(USArrests, USAccDeaths)
  myf <- function(...) list(...)

  ans <- suppressMessages(suppressWarnings(
    Slurm_Map(function(x,y) list(x=x, y=y), njobs=2, mc.cores = 1L, x=a, y=b,
              export = "myf")
    )
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], list(x=a[[1]], y=b[[1]]))

})
