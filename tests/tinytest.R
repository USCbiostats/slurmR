
Sys.setenv(R_TESTS = "")

if ( requireNamespace("tinytest", quietly=TRUE) & (.Platform$OS.type == "unix") ){
  tinytest::test_package("slurmR")
}

