suppressMessages({
  opts_slurmR$debug_on()
  opts_slurmR$verbose_off()
})

# Exported objects cannot clash with reserved names (issue #49) ---------------
seeds <- 1:2
expect_error(
  Slurm_EvalQ(
    seeds,
    njobs    = 2,
    export   = "seeds",
    plan     = "none",
    job_name = "test-Slurm_EvalQ-clash"
  ),
  pattern = "seeds"
)
expect_false(
  dir.exists(file.path(opts_slurmR$get_tmp_path(), "test-Slurm_EvalQ-clash"))
)

# Exporting a non-reserved name works
myobj <- 1:3
job <- Slurm_EvalQ(
  sum(myobj),
  njobs    = 2,
  export   = "myobj",
  plan     = "none",
  job_name = "test-Slurm_EvalQ-export"
)
expect_true(file.exists(job$rscript))

suppressMessages({
  opts_slurmR$debug_off()
})
