# Issue #44: flags without a value, short options, and values with `=` -------
tmp_dir <- tempfile("read_sbatch")
dir.create(tmp_dir)
rscript <- file.path(tmp_dir, "issue44.R")

writeLines(
  c(
    "#!/bin/sh",
    "#SBATCH --exclusive",
    "#SBATCH --requeue",
    "#SBATCH --time=01:00:00",
    "#SBATCH --mem-per-cpu 4G",
    "#SBATCH --export=ALL,FOO=bar",
    "#SBATCH --comment=\"has spaces\"",
    "#SBATCH -N 2",
    "#SBATCH -c4",
    "#SBATCH --hold",
    "print(\"Ok\")"
  ),
  rscript
)

# Reading the options ----------------------------------------------------------
opts <- read_sbatch(rscript)

expect_true(is.list(opts))
expect_equal(
  names(opts),
  c(
    "exclusive", "requeue", "time", "mem-per-cpu", "export", "comment", "N",
    "c", "hold"
  )
)
expect_identical(opts$exclusive, TRUE)
expect_identical(opts$requeue, TRUE)
expect_identical(opts$hold, TRUE)
expect_identical(opts$time, "01:00:00")
expect_identical(opts$`mem-per-cpu`, "4G")
expect_identical(opts$export, "ALL,FOO=bar")
expect_identical(opts$comment, "has spaces")
expect_identical(opts$N, "2")
expect_identical(opts$c, "4")

# Round trip through parse_flags
expect_equal(
  parse_flags(opts),
  c(
    "--exclusive", "--requeue", "--time=01:00:00", "--mem-per-cpu=4G",
    "--export=ALL,FOO=bar", "--comment=\"has spaces\"", "-N 2", "-c 4",
    "--hold"
  )
)

# Same result when passing the lines directly
expect_identical(read_sbatch(readLines(rscript)), opts)

# No options
expect_identical(read_sbatch(c("#!/bin/sh", "print(1)")), list())

# The example file
ex <- read_sbatch(system.file("example.slurm", package = "slurmR"))
expect_identical(ex$`job-name`, "NameOfTheJob")
expect_identical(ex$array, "1-100")

# End-to-end: sourceSlurm writes the bash file without submitting ------------
suppressWarnings(suppressMessages(
  sourceSlurm(rscript, tmp_path = tmp_dir, plan = "none")
))

sh <- readLines(file.path(tmp_dir, "issue44.R.sh"))
sbatch_lines <- sh[grepl("^#SBATCH", sh)]

expect_true("#SBATCH --exclusive" %in% sbatch_lines)
expect_true("#SBATCH --requeue" %in% sbatch_lines)
expect_true("#SBATCH --hold" %in% sbatch_lines)
expect_true("#SBATCH --time=01:00:00" %in% sbatch_lines)
expect_true("#SBATCH --mem-per-cpu=4G" %in% sbatch_lines)
expect_true("#SBATCH --export=ALL,FOO=bar" %in% sbatch_lines)
expect_true("#SBATCH --comment=\"has spaces\"" %in% sbatch_lines)
expect_true("#SBATCH -N 2" %in% sbatch_lines)
expect_true("#SBATCH -c 4" %in% sbatch_lines)
expect_true("#SBATCH --job-name=issue44.R" %in% sbatch_lines)
expect_false(any(grepl("--exclusive=", sbatch_lines, fixed = TRUE)))

# The generated file can itself be read back
sh_opts <- read_sbatch(file.path(tmp_dir, "issue44.R.sh"))
expect_identical(sh_opts$exclusive, TRUE)
expect_identical(sh_opts$`job-name`, "issue44.R")

# sbatch on the generated file (without submitting) works with the list
# returned by read_sbatch
expect_warning(
  sbatch(file.path(tmp_dir, "issue44.R.sh"), submit = FALSE),
  "hasn't been submitted"
)

unlink(tmp_dir, recursive = TRUE)
