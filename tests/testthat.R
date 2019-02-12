library(testthat)
library(sluRm)

# We need to set this if we have an issue?
Sys.setenv(SLURM_TEST=1)

test_check("sluRm")
