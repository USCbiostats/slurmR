if (!slurm_available()) {
  opts_slurmR$debug_on()
  opts_slurmR$set_tmp_path("/staging/ggv/")
} else {
  opts_slurmR$set_tmp_path(tempdir())
}

expect_message(
  suppressWarnings(sourceSlurm(system.file("example.R", package="slurmR"), plan = "submit",
    job_name = "test-sourceSlurm", partition="scavenge", time = "01:00:00")),
  ".*Sourcing an R script using Slurm.*"
)

if (!slurm_available())
  opts_slurmR$debug_off()



