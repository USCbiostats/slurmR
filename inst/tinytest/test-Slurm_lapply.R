
suppressMessages({
  opts_slurmR$debug_on()
  opts_slurmR$verbose_off()
})

b <- list(1:5, 1:10)

ans <- suppressMessages(suppressWarnings(
  Slurm_lapply(
    b, mean,
    njobs    = 2L,
    mc.cores = 1L,
    plan     = "wait",
    job_name = "test-Slurm_lapply1"
  )
  ))

sol <- Slurm_collect(ans)

expect_equal(sol[[1]], 3)


b      <- list(1:100, 4:10)
mymean <- function(x) sum(x)/length(x)

ans <- suppressMessages(suppressWarnings(
  Slurm_lapply(
    b, function(z) mymean(z),
    njobs    = 2L,
    mc.cores = 1L,
    export   = "mymean",
    plan     = "wait",
    job_name = "test-Slurm_lapply2"
    )
  )
  )
ans0 <-  suppressMessages(suppressWarnings(
  Slurm_sapply(
    b, function(z) mymean(z),
    njobs    = 2,
    mc.cores = 1L,
    export   = "mymean",
    plan     = "wait",
    job_name = "test-Slurm_lapply3",
    simplify = FALSE)
  )
  )

# These two should be equivalent
expect_equal(Slurm_collect(ans0), Slurm_collect(ans))

# Checking printing and reading
expect_silent(print(ans))
ans1 <- read_slurm_job(paste0(opts_slurmR$get_tmp_path(), "/", "test-Slurm_lapply2"))
tmpjob <- tempfile()
write_slurm_job(ans, tmpjob)
ans2 <- read_slurm_job(tmpjob)
expect_equal(ans, ans1)
expect_equal(ans, ans2)


s <- status(ans)
expect_equal(as.integer(print(s)), -1L)

sol <- Slurm_collect(ans)

expect_equal(sol[[1]], mean(b[[1]]))

expect_warning(
  Slurm_lapply(
    list(1),
    mean,
    njobs    = 2,
    plan     = "none",
    job_name = "test-Slurm_lapply4"
  ),
  "length"
  )

expect_error(
  Slurm_lapply(
    list(1),
    mean,
    njobs = 1, 
    4,
    plan     = "none",
    job_name = "test-Slurm_lapply5"
  ),
  "unname"
  )

# Exported objects cannot clash with reserved names (issue #49) ---------------
nclash <- 0L
for (v in c("seeds", "X", "FUN", "mc.cores", "INDICES", "tcq", "ARRAY_ID")) {
  nclash <- nclash + 1L
  jname  <- paste0("test-Slurm_lapply-clash", nclash)
  assign(v, 1)
  expect_error(
    Slurm_lapply(
      list(1, 2),
      mean,
      njobs    = 2,
      export   = c("b", v),
      plan     = "none",
      job_name = jname
    ),
    pattern = v
  )

  # Nothing should be written to disk
  expect_false(dir.exists(file.path(opts_slurmR$get_tmp_path(), jname)))
}

# Clashes with arguments passed via ...
expect_error(
  Slurm_lapply(
    list(1, 2),
    function(x, na.rm) mean(x, na.rm = na.rm),
    na.rm    = TRUE,
    njobs    = 2,
    export   = "na.rm",
    plan     = "none",
    job_name = "test-Slurm_lapply-clash-dots"
  ),
  pattern = "na.rm"
)

# Slurm_sapply goes through Slurm_lapply
expect_error(
  Slurm_sapply(
    list(1, 2),
    mean,
    njobs    = 2,
    export   = "seeds",
    plan     = "none",
    job_name = "test-Slurm_sapply-clash"
  ),
  pattern = "seeds"
)

# The reserved names must cover every object the R script assigns at the top
# level (besides `ans` and exported objects). This guards against adding new
# objects to the script without updating `RSCRIPT_RESERVED_NAMES`.
top_level_assignments <- function(e) {

  if (is.expression(e) || (is.call(e) && identical(e[[1]], as.name("{"))))
    return(unlist(lapply(as.list(e), top_level_assignments)))

  if (!is.call(e))
    return(NULL)

  fun <- e[[1]]
  if (identical(fun, as.name("function")))
    return(NULL)

  if (
    (identical(fun, as.name("<-")) || identical(fun, as.name("="))) &&
      is.name(e[[2]])
  )
    return(c(as.character(e[[2]]), top_level_assignments(e[[3]])))

  unlist(lapply(as.list(e)[-1], top_level_assignments))

}

job <- Slurm_lapply(
  list(1, 2),
  function(x) mymean(x),
  njobs    = 2,
  export   = "mymean",
  plan     = "none",
  job_name = "test-Slurm_lapply-reserved"
)

assigned <- unique(top_level_assignments(parse(file = job$rscript)))
reserved <- c(
  slurmR:::RSCRIPT_RESERVED_NAMES, "INDICES", "X", "FUN", "mc.cores"
)
expect_equal(
  sort(setdiff(assigned, c("ans", "mymean"))),
  sort(reserved)
)

# If slurm is available


# Default debug option
suppressMessages({
  opts_slurmR$debug_off()
})

if (slurm_available()) {

  set.seed(1231512)
  x <- runif(100)
  ans0 <- Slurm_lapply(
    x, mean, njobs = 4, job_name = "test-Slurm_lapply6"
  )
  ans1 <- lapply(x, mean)

  expect_equal(ans0, ans1)
}


