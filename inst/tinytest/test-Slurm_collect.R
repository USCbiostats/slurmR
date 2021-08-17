if (slurm_available()) {

  # Checking if we get the right error message
  set.seed(12315)
  z <- runif(20)

  x <- Slurm_lapply(
    1:20, function(x) mean(z*x),
    export   = "z",
    plan     = "none",
    njobs    = 2L,
    job_name = "test-Slurm_collect1"
  )

  removed <- file.remove(
    paste0(x$opts_r$tmp_path, "/", x$opts_job$`job-name`,"/z.rds")
  )

  sbatch(x, wait = TRUE)
  ans <- Slurm_collect(x)
  Slurm_clean(x)
  expect_true(grepl("An error has oc", ans[[1]]$slurmr_msg))

  # Collecting jobs with different runs
  x <- Slurm_lapply(
    1:20, mean,
    njobs    = 2L,
    plan     = "none",
    job_name = "test-Slurm_collect2"
  )

  sbatch(x, array = 1, wait = TRUE)
  sbatch(x, array = 2, wait = TRUE)
  ans <- Slurm_collect(x)
  expect_equal(sort(unlist(ans)), 1:20)
  # Slurm_clean(x)

  # Collecting any
  x <- Slurm_lapply(
    1:10,
    function(i) {
      if (Slurm_env("SLURM_ARRAY_TASK_ID") == 1)
        Sys.sleep(1e3)
      else
        i
    },
    njobs    = 2,
    job_name = "test-Slurm_collect3",
    plan     = "none",
    overwrite = TRUE
  )
 
  sbatch(x, array = 2, wait = TRUE)
  sbatch(x, array = 1, wait = FALSE)
  Sys.sleep(5)
  scancel(x)
  ans <- Slurm_collect(x, any. = TRUE)
  # Slurm_clean(x)


}


