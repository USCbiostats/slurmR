#' Read a slurm batch file and capture the SBATCH options
#'
#' @param x Character scalar. Either the path to the batch file to process, or
#' a character vector.
#' @return A named list of the options starting with `#SBATCH` in the file. The
#' names are the option names without leading dashes (e.g., `"job-name"` for
#' `--job-name`, or `"J"` for `-J`). Options with a value (e.g.,
#' `--time=01:00:00`, `--time 01:00:00`, or `-N 2`) are stored as character
#' scalars, while flags without a value (e.g., `--exclusive`) are stored as
#' `TRUE`, so the list can be passed to [parse_flags()] to recover the options.
#' If no option is found, then returns an empty list.
#' @examples
#'
#' # Reading in an example script
#' x <- system.file("example.slurm", package="slurmR")
#' read_sbatch(x)
#'
#' @export
read_sbatch <- function(x) {

  dat <- if (length(x) == 1 && file.exists(x))
    unlist(strsplit(readLines(x), split = "\n"))
  else x

  # Keeping everything
  dat <- dat[grepl("^(#SBATCH|#!/bin/|[^#])", dat, perl = TRUE)]

  # Tagging SBATCH options
  opts_ids <- which(grepl("^#SBATCH", dat))

  if (!length(opts_ids))
    return(list())

  opts <- dat[opts_ids]
  opts <- gsub("^#SBATCH\\s+|\\s+$", "", opts, perl = TRUE)

  # Long options: --name, --name=value, or --name value
  # Short options: -n, -n value, -nvalue, or -n=value
  # Only the first separator is used, so values with `=` (e.g.,
  # --export=ALL,FOO=bar) are kept as is.
  pat <- ifelse(
    grepl("^--", opts),
    "^--([^=\\s]+)(?:(?:=|\\s+)(.*))?$",
    "^-([^-=\\s])(?:=|\\s*)(.*)$"
  )

  opts_names <- character(length(opts))
  ans        <- vector("list", length(opts))
  for (i in seq_along(opts)) {

    m <- regmatches(opts[i], regexec(pat[i], opts[i], perl = TRUE))[[1]]

    # Not a recognizable option, so we skip it
    if (!length(m))
      next

    opts_names[i] <- m[2]

    # Removing surrounding quotes (parse_flags adds them back if needed)
    val <- gsub("^\"(.*)\"$|^'(.*)'$", "\\1\\2", m[3], perl = TRUE)

    # Flags without a value, e.g., --exclusive, are stored as TRUE
    ans[[i]] <- if (is.na(val) || val == "") TRUE else val

  }

  keep <- opts_names != ""

  structure(ans[keep], names = opts_names[keep])
}
