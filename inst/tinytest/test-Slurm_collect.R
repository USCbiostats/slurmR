if (slurm_available()) {
  
  # Checking if we get the right error message
  opts_slurmR$set_opts(partition = "scavenge")
  opts_slurmR$set_tmp_path("/staging/ggv/")

  set.seed(12315)
  z <- runif(20)
  x <- Slurm_lapply(1:20, function(x) mean(z*x), export = "z",
    plan = "none", njobs = 2L, job_name = "test-Slurm_collect1")
  removed <- file.remove(
    paste0(x$opts_r$tmp_path, "/", x$opts_job$`job-name`,"/z.rds")
  )
  sbatch(x)
  ans <- Slurm_collect(x)
  Slurm_clean(x)
  expect_true(grepl("An error has oc", ans[1]$message))

  # Collecting jobs with different runs
  x <- Slurm_lapply(1:20, mean, njobs = 2L, plan = "none", job_name = "test-Slurm_collect2")
  sbatch(x, array = 1, wait = TRUE)
  sbatch(x, array = 2, wait = TRUE)
  ans <- Slurm_collect(x)
  expect_equal(sort(unlist(ans)), 1:20)
  Slurm_clean(x)
}
