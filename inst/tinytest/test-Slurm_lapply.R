  # Setting it up
x <- ifelse(slurm_available(), "/staging/ggv/", tempdir())

suppressMessages({
  opts_slurmR$set_tmp_path(x)
  opts_slurmR$debug_on()
  opts_slurmR$verbose_off()
  opts_slurmR$set_opts(partition="scavenge")
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
ans0 <-  suppressMessages(suppressWarnings(
  Slurm_sapply(b, function(z) mymean(z), njobs=2, mc.cores = 1L, export="mymean",
               plan = "wait", job_name = "test-lapply3", simplify = FALSE)
  )
  )

# These two should be equivalent
expect_equal(Slurm_collect(ans0), Slurm_collect(ans))

# Checking printing and reading
expect_silent(print(ans))
ans1 <- read_slurm_job(paste0(opts_slurmR$get_tmp_path(), "/", "test-lapply2"))
tmpjob <- tempfile()
write_slurm_job(ans, tmpjob)
ans2 <- read_slurm_job(tmpjob)
expect_equal(ans, ans1)
expect_equal(ans, ans2)


s <- status(ans)
expect_equal(as.integer(print(s)), -1L)

sol <- Slurm_collect(ans)

expect_equal(sol[[1]], mean(b[[1]]))

expect_warning(Slurm_lapply(list(1), mean, njobs = 2, plan = "none", job_name = "test-lapply3"), "length")
expect_error(Slurm_lapply(list(1), mean, njobs = 1, 4, plan = "none", job_name = "test-lapply4"), "unname")

# If slurm is available


# Default debug option
suppressMessages({
  opts_slurmR$debug_off()
  opts_slurmR$set_opts(partition=NULL)
})

if (slurm_available()) {

  set.seed(1231512)
  x <- runif(100)
  ans0 <- Slurm_lapply(
    x, mean, njobs = 4, job_name = "test-Slurm_lapply5",
    sbatch_opt = list(partition = "scavenge")
  )
  ans1 <- lapply(x, mean)

  expect_equal(ans0, ans1)
}
