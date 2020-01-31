
Sys.setenv(R_TESTS = "")
if ( requireNamespace("tinytest", quietly=TRUE) & (.Platform$OS.type == "unix") ) {

  library(slurmR)
  if (slurm_available()) {
    opts_slurmR$set_tmp_path("/staging/ggv/slurmR-tinytest/")
    opts_slurmR$set_opts(partition = "thomas", account = "lc_pdt")
  } else {
    opts_slurmR$set_tmp_path(tempdir())
  }
  tinytest::test_package("slurmR")
}

