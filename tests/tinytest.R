
Sys.setenv(R_TESTS = "")
Sys.setenv(R_SLURMR_TEST = "TRUE")

if ( requireNamespace("tinytest", quietly=TRUE) & (.Platform$OS.type == "unix") ){
  tinytest::test_package("slurmR")
}

Sys.unsetenv("R_SLURMR_TEST")
