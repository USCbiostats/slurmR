
Sys.setenv(R_TESTS = "")
if ( requireNamespace("tinytest", quietly=TRUE) & (.Platform$OS.type == "unix") ) {

  library(slurmR)
  if (slurm_available()) {
    opts_slurmR$set_tmp_path("/scratch/vegayon/ggv/slurmR-rcmdcheck/")
    opts_slurmR$set_opts(account = "pdthomas_136")
  } else {
    opts_slurmR$set_tmp_path(tempdir())
  }
  tinytest::test_package("slurmR")
}

