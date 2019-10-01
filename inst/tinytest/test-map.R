  # Setting it up
 x <- ifelse(slurm_available(), "/staging/ggv/", tmpdir())

  suppressMessages({
    opts_slurmR$set_tmp_path(x)
    opts_slurmR$debug_on()
    opts_slurmR$verbose_off()
  })

  b <- list(1:5, 1:10)
  a <- list(USArrests, USAccDeaths)

  ans <- suppressMessages(suppressWarnings(
    Slurm_Map(function(x,y) list(x=x, y=y), njobs=2, mc.cores = 1L, x=a, y=b,
              plan = "wait", job_name = "test-map1"))
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], list(x=a[[1]], y=b[[1]]))

  b <- list(1:5, 1:10)
  a <- list(USArrests, USAccDeaths)
  myf <- function(...) list(...)

  ans <- suppressMessages(suppressWarnings(
    Slurm_Map(function(x,y) list(x=x, y=y), njobs=2, mc.cores = 1L, x=a, y=b,
              export = "myf", plan = "wait", job_name = "test-map2")
    )
    )
  sol <- Slurm_collect(ans)

  expect_equal(sol[[1]], list(x=a[[1]], y=b[[1]]))

  expect_warning(Slurm_Map(function(x,y) mean(x,y), x = 1, y = 1:2, njobs = 4, plan = "none", job_name = "test-map3"), "length")
  expect_error(Slurm_Map(function(x) mean(x), x = list(1), 4, njobs = 2, plan = "none", job_name = "test_map4"), "unname")

suppressMessages(
  opts_slurmR$debug_off()
)

# })
