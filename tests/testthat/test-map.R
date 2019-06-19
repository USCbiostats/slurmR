context("Map functions")

test_that("Works in debug mode", {

  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_tmp_path(x)
    opts_sluRm$set_job_name("map-1")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  b <- list(1:5, 1:10)
  a <- list(USArrests, USAccDeaths)

  ans <- suppressMessages(suppressWarnings(
    Slurm_Map(function(x,y) list(x=x, y=y), njobs=2, mc.cores = 1L, x=a, y=b,
              plan = "wait"))
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], list(x=a[[1]], y=b[[1]]))

})


test_that("Exporting", {

  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_tmp_path(x)
    opts_sluRm$set_job_name("map-3")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  b <- list(1:5, 1:10)
  a <- list(USArrests, USAccDeaths)
  myf <- function(...) list(...)

  ans <- suppressMessages(suppressWarnings(
    Slurm_Map(function(x,y) list(x=x, y=y), njobs=2, mc.cores = 1L, x=a, y=b,
              export = "myf", plan = "wait")
    )
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], list(x=a[[1]], y=b[[1]]))

})


test_that("Warnings and errors", {

  x <- tempdir()
  suppressMessages({
    opts_sluRm$set_tmp_path(x)
    opts_sluRm$set_job_name("map-warns-and-errors")
    opts_sluRm$debug_on()
    opts_sluRm$verbose_off()
  })

  expect_warning(Slurm_Map(function(x,y) mean(x,y), x = 1, y = 1:2, njobs = 4, plan = "none"), "length")
  expect_error(Slurm_Map(function(x) mean(x), x = list(1), 4, njobs = 2, plan = "none"), "unname")

})
