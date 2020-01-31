
Sys.setenv(R_TESTS = "")
if ( requireNamespace("tinytest", quietly=TRUE) & (.Platform$OS.type == "unix") ) {

  library(slurmR)
  opts_slurmR$set_tmp_path("/staging/ggv/slurmR-tinytest/")
  opts_slurmR$set_opts(partition = "thomas", account = "lc_pdt")
  tinytest::test_package("slurmR")
}


