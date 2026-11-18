# Issue #43: library() calls in the job script must respect `libPaths`, even
# when the detected location of a package is not a library tree (as happens
# with the renv cache).
tmp      <- tempfile("test-rscript-libpaths")
job_name <- "rscript-libpaths"
dir.create(file.path(tmp, job_name), recursive = TRUE)

# A renv-like cache directory: the package lives under
# <cache>/<pkg>/<version>/<hash>/<pkg>, so <cache> is not a library.
fake_cache <- file.path(tmp, "renv-cache")
dir.create(
  file.path(fake_cache, "tinytest", "1.0.0", "abc123", "tinytest"),
  recursive = TRUE
)

lib_custom <- file.path(tmp, "custom-lib")
dir.create(lib_custom)
libs <- c(lib_custom, .libPaths())

rscript <- slurmR:::new_rscript(
  njobs    = 1L,
  tmp_path = tmp,
  job_name = job_name,
  pkgs     = list(tinytest = fake_cache),
  libPaths = libs
)
rscript$finalize("ans")
rscript$write()

fn <- file.path(tmp, job_name, "00-rscript.r")
expect_true(file.exists(fn))

script   <- readLines(fn)
lib_line <- grep("library(tinytest", script, fixed = TRUE, value = TRUE)
expect_equal(length(lib_line), 1L)

# The job script sets the library paths
expect_true(any(grepl(
  sprintf(".libPaths(c(\"%s\"))", paste(libs, collapse = "\", \"")),
  script,
  fixed = TRUE
)))

# The library() call searches the detected location first, then libPaths
expect_true(grepl(
  sprintf(
    "lib.loc = c(\"%s\")",
    paste(c(fake_cache, libs), collapse = "\", \"")
  ),
  lib_line,
  fixed = TRUE
))

# The generated call works: it falls back to libPaths since the detected
# location is not a library tree.
expr <- parse(text = sub("^\\s*tcq\\(\\{\\s*|\\s*\\}\\)\\s*$", "", lib_line))
expect_silent(eval(expr[[1]], envir = new.env()))

# Duplicated paths are not repeated (the detected library stays first)
calls <- slurmR:::load_packages(
  list(stats = libs[2]),
  tmp_path = tmp,
  job_name = job_name,
  libPaths = libs
)
expected <- paste(c(libs[2], libs[-2]), collapse = "\", \"")
expect_equal(
  calls,
  sprintf("library(stats, lib.loc = c(\"%s\"))", expected)
)

# Without packages, no library() calls are generated
expect_equal(
  length(slurmR:::load_packages(list(), tmp, job_name, libs)),
  0L
)

unlink(tmp, recursive = TRUE)
