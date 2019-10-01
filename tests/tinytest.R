
Sys.setenv(R_TESTS = "")

if ( requireNamespace("tinytest", quietly=TRUE) ){
  tinytest::test_package("slurmR")
}

