if (!slurm_available())
  opts_slurmR$debug_on()

expect_message(
  suppressWarnings(sourceSlurm(system.file("example.R", package="slurmR"), plan = "submit",
    job_name = "test-sourceSlurm", partition="conti", account="lc_dvc", time = "01:00:00")),
  ".*Sourcing an R script using Slurm.*"
)

if (!slurm_available())
  opts_slurmR$debug_off()



