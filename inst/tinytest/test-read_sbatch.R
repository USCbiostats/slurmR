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

# Parsing follows Slurm's rules (src/sbatch/opt.c, get_argument) ---------------

# Several options in one line, and trailing comments
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --gres=gpu:1 --mem=4G  # a comment")),
  list(gres = "gpu:1", mem = "4G")
)
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH -N2 -n 4 --exclusive # note")),
  list(N = "2", n = "4", exclusive = TRUE)
)
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --exclusive # note", "#SBATCH --hold")),
  list(exclusive = TRUE, hold = TRUE)
)

# A # inside quotes, or escaped, is not a comment
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --comment=\"a # b\" --job-name=x\\#y")),
  list(comment = "a # b", `job-name` = "x#y")
)

# Quotes are removed (and can be nested); backslashes escape
expect_identical(
  read_sbatch(c(
    "#!/bin/sh",
    "#SBATCH --comment='say \"hi\" there'",
    "#SBATCH --export=ALL,X=\"a b\"",
    "#SBATCH -J \"my\\ job\""
  )),
  list(comment = "say \"hi\" there", export = "ALL,X=a b", J = "my job")
)

# Empty values are kept (they are not flags without values)
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --mem= --chdir=\"\" --hold")),
  list(mem = "", chdir = "", hold = TRUE)
)

# Slurm stops reading at the first command
expect_identical(
  read_sbatch(c(
    "#!/bin/sh", "", "# a comment", "#SBATCH --hold", "echo hi",
    "#SBATCH --exclusive"
  )),
  list(hold = TRUE)
)

# Round trip: what we read, is what we write (quoting/escaping included)
opts <- list(
  comment = "say \"hi\" there",
  chdir   = "",
  J       = "my job",
  export  = "ALL,X=a b",
  name    = "a#b",
  hold    = TRUE
)
flags <- parse_flags(opts)
expect_identical(
  read_sbatch(c("#!/bin/sh", paste("#SBATCH", flags))),
  opts
)

# The job name can be given as -J or --job-name (the last one wins)
expect_identical(
  slurmR:::normalize_job_name(list(J = "a", exclusive = TRUE)),
  list(exclusive = TRUE, `job-name` = "a")
)
expect_identical(
  slurmR:::normalize_job_name(list(`job-name` = "a", J = "b")),
  list(`job-name` = "b")
)
expect_identical(slurmR:::normalize_job_name(list(hold = TRUE)), list(hold = TRUE))

# sourceSlurm and sbatch pick the name given by -J
tmp_dir <- tempfile("read_sbatch")
dir.create(tmp_dir)
rscript <- file.path(tmp_dir, "short-name.R")
writeLines(c("#!/bin/sh", "#SBATCH -J myjob", "#SBATCH --hold", "print(1)"), rscript)

suppressWarnings(suppressMessages(
  sourceSlurm(rscript, tmp_path = tmp_dir, plan = "none")
))
sbatch_lines <- grep("^#SBATCH", readLines(file.path(tmp_dir, "short-name.R.sh")), value = TRUE)
expect_true("#SBATCH --job-name=myjob" %in% sbatch_lines)
expect_false(any(grepl("-J", sbatch_lines, fixed = TRUE)))
expect_false(any(grepl("short-name", sbatch_lines, fixed = TRUE)))

unlink(tmp_dir, recursive = TRUE)

# Flags without values ---------------------------------------------------------

# Clustered or repeated short flags
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH -Hs -vvv -n 4")),
  list(H = TRUE, s = TRUE, v = TRUE, v = TRUE, v = TRUE, n = "4")
)
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH -Hn4")),
  list(H = TRUE, n = "4")
)

# Options without a value (or with an optional one) never take the next argument
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --requeue --time 01:00:00 --exclusive=user")),
  list(requeue = TRUE, time = "01:00:00", exclusive = "user")
)
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --nice=-5 --hold")),
  list(nice = "-5", hold = TRUE)
)

# Only ASCII white space separates arguments (as in Slurm)
expect_identical(
  read_sbatch(c("#!/bin/sh", "#SBATCH --comment=a b -J x")),
  list(comment = "a b", J = "x")
)

# An empty job name is ignored
expect_identical(
  slurmR:::normalize_job_name(list(`job-name` = "", hold = TRUE)),
  list(hold = TRUE)
)

# Writing flags -----------------------------------------------------------------

# New lines cannot be written, and empty values of short options are dropped
expect_error(parse_flags(list(comment = "a\nb")), "new lines")
expect_warning(
  ans <- parse_flags(list(J = "", hold = TRUE)),
  "empty value"
)
expect_identical(ans, "--hold")

# Values reach the program as they are (shell metacharacters included), which
# we check by passing them through the shell
if (.Platform$OS.type == "unix") {

  values <- c(
    "haswell|skylake", "a;b", "a&b", "a<b>c", "(x)", "{a,b}", "node[1-3]",
    "*", "it's", "say \"hi\"", "back\\slash", "a b", "a#b", "plain-1.2/x:y,z=w"
  )
  for (v in values) {
    flag <- parse_flags(list(comment = v))
    out  <- system(paste("printf '%s\\n'", flag), intern = TRUE)
    expect_identical(out, paste0("--comment=", v), info = v)

    # And Slurm reads them back as well
    expect_identical(
      read_sbatch(c("#!/bin/sh", paste("#SBATCH", flag)))$comment,
      v,
      info = v
    )
  }

  # Variables are still expanded by the shell, as before
  expect_identical(
    system(paste("printf '%s\\n'", parse_flags(list(chdir = "$HOME/x"))), intern = TRUE),
    paste0("--chdir=", Sys.getenv("HOME"), "/x")
  )

}
