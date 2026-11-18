# Jobs submitted to another cluster (sbatch -M/--clusters).
#
# Slurm reports "Submitted batch job <id> on cluster <name>" and the job can
# only be seen by squeue/sacct/scancel by passing --clusters (-M) as well. The
# job object must remember its cluster, otherwise status(), wait_slurm() and
# scancel() look for the job in the wrong cluster (scancel() would even wait
# forever for a job that cannot be found).
#
# Since this does not require a Slurm installation, we simulate the Slurm
# commands with fake executables that follow the documented output formats:
# - sbatch prints the message in FAKE_SBATCH_MSG;
# - sacct only knows about the job when --clusters=B is passed;
# - squeue prints a "CLUSTER: <name>" line before the header when
#   --clusters is used;
# - all of them log their arguments in FAKE_SLURM_LOG.
# This only runs when Slurm is not installed so the fakes do not shadow it.
if ((.Platform$OS.type == "unix") && !slurm_available()) local({

  fake_bin <- tempfile("slurmr-fake-bin-")
  dir.create(fake_bin)
  log_file <- tempfile("slurmr-fake-log-")

  old_env <- Sys.getenv(c("PATH", "FAKE_SLURM_LOG", "FAKE_SBATCH_MSG"), NA)
  job_record <- slurmR:::LAST_SUBMITTED_JOB
  old_job    <- job_record$job
  on.exit({
    job_record$job <- old_job
    for (v in names(old_env)) {
      if (is.na(old_env[[v]]))
        Sys.unsetenv(v)
      else
        do.call(Sys.setenv, structure(list(old_env[[v]]), names = v))
    }
    unlink(c(fake_bin, log_file), recursive = TRUE)
  })

  script <- c(
    "#!/bin/sh",
    "echo \"$(basename \"$0\") $*\" >> \"$FAKE_SLURM_LOG\"",
    "case \"$(basename \"$0\")\" in",
    "  sbatch) echo \"$FAKE_SBATCH_MSG\" ;;",
    "  sacct)",
    "    case \"$*\" in",
    "      *--clusters=B*) printf 'JobID|State|ExitCode|\\n555|COMPLETED|0:0|\\n' ;;",
    "      *--clusters=Z*) printf 'JobID|State|ExitCode|\\n' ;;",
    "      *) echo 'sacct: error: no such job' >&2; exit 1 ;;",
    "    esac ;;",
    "  squeue)",
    "    case \"$*\" in",
    "      *--clusters=B*) printf 'CLUSTER: B\\nJOBID|STATUS|\\n555_1|RUNNING|\\n' ;;",
    "      *) printf 'JOBID|STATUS|\\n777_1|RUNNING|\\n' ;;",
    "    esac ;;",
    "esac"
  )
  for (cmd in c("sbatch", "sacct", "squeue", "scancel")) {
    f <- file.path(fake_bin, cmd)
    writeLines(script, f)
    Sys.chmod(f, mode = "0755")
  }
  Sys.setenv(
    PATH           = paste(fake_bin, old_env[["PATH"]], sep = .Platform$path.sep),
    FAKE_SLURM_LOG = log_file
  )
  expect_true(slurm_available())

  last_call <- function(cmd) {
    l <- grep(paste0("^", cmd, " "), readLines(log_file), value = TRUE)
    l[length(l)]
  }

  new_job <- function(name) Slurm_lapply(
    1:2, function(i) i, njobs = 2L, plan = "none", tmp_path = tempdir(),
    job_name = name
  )

  # Submitting to another cluster ---------------------------------------------
  Sys.setenv(FAKE_SBATCH_MSG = "Submitted batch job 555 on cluster B")
  job <- new_job("test-cluster-remote")
  expect_null(job$cluster)

  suppressMessages(sbatch(job, clusters = "B"))
  expect_equal(job$jobid, 555L)
  expect_equal(job$cluster, "B")
  expect_true(grepl("--clusters=B", last_call("sbatch"), fixed = TRUE))

  # The cluster is saved with the job
  expect_equal(read_slurm_job(dirname(job$bashfile))$cluster, "B")

  # status() and wait_slurm() use the cluster of the job
  expect_equal(as.integer(status(job)), 0L)
  expect_true(grepl("--clusters=B", last_call("sacct"), fixed = TRUE))
  expect_silent(wait_slurm(job, timeout = 10))

  # For job ids, the cluster has to be passed
  expect_equal(as.integer(status(555L, clusters = "B")), 0L)
  expect_silent(wait_slurm(555L, clusters = "B", timeout = 10))

  # A job that cannot be found does not make us wait beyond the timeout
  expect_warning(
    wait_slurm(555L, clusters = "Z", timeout = 1),
    "Timeout"
  )

  # squeue ignores the "CLUSTER: <name>" line
  q <- squeue(job)
  expect_equal(names(q), c("JOBID", "STATUS"))
  expect_equal(q$JOBID, "555_1")
  expect_true(grepl("--clusters=B", last_call("squeue"), fixed = TRUE))

  # The cluster can be overridden
  expect_error(sacct(job, clusters = "C"))
  expect_true(grepl("--clusters=C", last_call("sacct"), fixed = TRUE))

  # scancel() looks for the job (and waits for it) in the right cluster. If it
  # looked in the wrong one, it would wait forever, so we set a time limit.
  res <- tryCatch({
    setTimeLimit(elapsed = 20, transient = TRUE)
    scancel(job)
    NULL
  }, error = function(e) e)
  setTimeLimit(elapsed = Inf, transient = TRUE)
  expect_null(res)
  expect_true(grepl("--clusters=B", last_call("scancel"), fixed = TRUE))
  expect_silent(scancel(555L, clusters = "B"))

  # sbatch on a file also waits on the right cluster
  expect_equal(suppressMessages(sbatch(job$bashfile, wait = TRUE)), 555L)

  # A cluster name with digits (see #59) ---------------------------------------
  Sys.setenv(FAKE_SBATCH_MSG = "Submitted batch job 555 on cluster B")
  job2 <- new_job("test-cluster-remote2")
  suppressMessages(sbatch(job2))
  expect_equal(job2$cluster, "B")

  # Local submissions have no cluster -----------------------------------------
  Sys.setenv(FAKE_SBATCH_MSG = "Submitted batch job 777")
  job3 <- new_job("test-cluster-local")
  suppressMessages(sbatch(job3))
  expect_equal(job3$jobid, 777L)
  expect_null(job3$cluster)

  q <- squeue(job3)
  expect_equal(q$JOBID, "777_1")
  expect_false(grepl("--clusters", last_call("squeue"), fixed = TRUE))

  # Resubmitting locally drops the old cluster
  job$jobid <- NA_integer_
  suppressMessages(sbatch(job))
  expect_equal(job$jobid, 777L)
  expect_null(job$cluster)

})
