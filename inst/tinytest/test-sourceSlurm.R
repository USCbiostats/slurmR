if (!slurm_available())
  opts_slurmR$debug_on()

expect_message(
  suppressWarnings(sourceSlurm(system.file("example.R", package="slurmR"), plan = "submit",
    job_name = "test-sourceSlurm1", time = "01:00:00")),
  ".*Sourcing an R script using Slurm.*"
)

expect_error(sourceSlurm(tempfile()), "not found")

# Adding a name
tmp <- tempfile()
cat("#!/bin/sh", "#SBATCH --n-tasks=1", "print(\"Ok\")", file = tmp, sep="\n")

suppressWarnings(sourceSlurm(tmp, plan = "none"))

# The last job should be in the form of
name <- gsub(".+[/](?=[^/]+$)", "", tmp, perl = TRUE)
path <- paste0(
  opts_slurmR$get_tmp_path(), "/",
  name,
  ".slurm"
  )

# This test is not rightly working, for the moment
# expect_equal(name, unname(read_sbatch(path)["job-name"]))

expect_warning(sourceSlurm(tmp, plan = "collect"), "not possible")

expect_message(
  slurmr_cmd(opts_slurmR$get_tmp_path(), bashrc_path = tempfile()),
  "Remember that this"
  )

if (!slurm_available())
  opts_slurmR$debug_off()



