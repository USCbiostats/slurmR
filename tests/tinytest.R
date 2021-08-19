
Sys.setenv(R_TESTS = "")
if ( requireNamespace("tinytest", quietly=TRUE) & (.Platform$OS.type == "unix") ) {

  library(slurmR)

  # Figuringout the directory
  SLURMR_TEST_DIR <- Sys.getenv("SLURMR_TEST_DIR")
  if (SLURMR_TEST_DIR == "")
    SLURMR_TEST_DIR <- NULL
  

  if (slurm_available()) {
    opts_slurmR$set_tmp_path(SLURMR_TEST_DIR)
    # opts_slurmR$set_opts(account = "pdthomas_136")
  } else {
    opts_slurmR$set_tmp_path(tempdir())
  }
  tinytest::test_package("slurmR")
}

