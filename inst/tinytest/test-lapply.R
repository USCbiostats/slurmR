  # Setting it up
  x <- tempdir()
  suppressMessages({
    opts_slurmR$set_tmp_path(x)
    opts_slurmR$debug_on()
    opts_slurmR$verbose_off()
  })

  b <- list(1:5, 1:10)

  ans <- suppressMessages(suppressWarnings(
    Slurm_lapply(b, mean, njobs=2, mc.cores = 1L, plan = "wait", job_name = "test-lapply1")))
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], 3)


  b      <- list(1:100, 4:10)
  mymean <- function(x) sum(x)/length(x)

  ans <- suppressMessages(suppressWarnings(
    Slurm_lapply(b, function(z) mymean(z), njobs=2, mc.cores = 1L, export="mymean",
                 plan = "wait", job_name = "test-lapply2")
    )
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], mean(b[[1]]))

  expect_warning(Slurm_lapply(list(1), mean, njobs = 2, plan = "none", job_name = "test-lapply3"), "length")
  expect_error(Slurm_lapply(list(1), mean, njobs = 1, 4, plan = "none", job_name = "test-lapply4"), "unname")

# Default debug option
suppressMessages(
  opts_slurmR$debug_off()
)

