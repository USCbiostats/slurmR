# Parsing the job id from the output of sbatch (does not require Slurm)
parse_jobid <- slurmR:::parse_sbatch_jobid

# Standard output
expect_identical(parse_jobid("Submitted batch job 8653762"), 8653762L)

# Federated clusters (issue #41)
expect_identical(
  parse_jobid("Submitted batch job 8653762 on cluster name_of_cluster"),
  8653762L
)

# Extra lines before/after the relevant one (e.g., warnings from sbatch)
expect_identical(
  parse_jobid(
    c(
      "sbatch: some informational message",
      "Submitted batch job 123 on cluster foo",
      ""
    )
  ),
  123L
)

# Informative error when no job id can be found
expect_error(
  parse_jobid("sbatch: error: Batch job submission failed"),
  "Unable to parse the job id"
)
expect_error(parse_jobid(character(0)), "Unable to parse the job id")
