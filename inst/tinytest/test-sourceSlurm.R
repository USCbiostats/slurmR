if (!slurm_available())
  opts_slurmR$debug_on()

expect_message(
  sourceSlurm(system.file("example.R", package="slurmR"), plan = "none"),
  "Sourcing an R script using Slurm"
)

if (!slurm_available())
  opts_slurmR$debug_off()



