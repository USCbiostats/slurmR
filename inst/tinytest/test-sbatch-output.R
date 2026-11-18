# Parsing the job id from the output of sbatch (does not require Slurm) ------
parse_jobid <- slurmR:::parse_sbatch_jobid

# Standard output
expect_identical(parse_jobid("Submitted batch job 8653762"), 8653762L)

# Cross-cluster submissions append the cluster name (issue #59), which may
# contain digits
expect_identical(
  parse_jobid("Submitted batch job 17159340 on cluster B"),
  17159340L
)
expect_identical(
  parse_jobid("Submitted batch job 17159340 on cluster cluster2"),
  17159340L
)

# Other lines (stderr is captured too) and the `sbatch: ` prefix shown in the
# Slurm documentation
expect_identical(
  parse_jobid(c("sbatch: warning 999", "Submitted batch job 123 on cluster foo", "")),
  123L
)
expect_identical(parse_jobid("sbatch: Submitted batch job 65541"), 65541L)

# Informative errors when there is no job id
expect_error(
  parse_jobid("sbatch: error: Batch job submission failed"),
  "Unable to parse the job id"
)
expect_error(parse_jobid(character(0)), "Unable to parse the job id")

# Real submission (only within a Slurm environment) --------------------------
# Set SLURMR_TEST_CLUSTER to the name of another cluster to also exercise a
# cross-cluster submission, i.e., the sbatch output ends with `on cluster <name>`
if (slurm_available()) {

  # Local submission
  job <- Slurm_EvalQ(
    Sys.sleep(1000), njobs = 1L, plan = "submit",
    job_name = "test-sbatch-output1"
  )

  expect_true(is.integer(job$jobid) && !is.na(job$jobid))
  expect_equal(last_job()$jobid, job$jobid)

  # Submitting through the bash file returns a valid job id as well
  jobid <- suppressMessages(sbatch(job$bashfile))
  expect_true(is.integer(jobid) && !is.na(jobid))

  scancel(job)
  scancel(jobid)
  Slurm_clean(job)

  # Cross-cluster submission (opt-in)
  cl <- Sys.getenv("SLURMR_TEST_CLUSTER")
  if (nzchar(cl)) {

    job <- Slurm_EvalQ(
      Sys.sleep(1000), njobs = 1L, plan = "submit",
      job_name   = "test-sbatch-output2",
      sbatch_opt = list(clusters = cl)
    )

    expect_true(is.integer(job$jobid) && !is.na(job$jobid))

    jobid <- do.call(sbatch, list(job$bashfile, clusters = cl))
    expect_true(is.integer(jobid) && !is.na(jobid))

    # scancel() is not cluster-aware, so we call it directly
    for (j in c(job$jobid, jobid))
      system2("scancel", c("-M", cl, j), stdout = FALSE, stderr = FALSE)
    Slurm_clean(job)

  }

}
