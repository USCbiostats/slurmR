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
#'
#' @details The options are read the way Slurm does it: only the `#SBATCH`
#' lines located before the first command are used; a line can have more than
#' one option (e.g., `#SBATCH -N2 -n4`); quotes group characters (and are
#' removed) and a backslash escapes the next character; and an unquoted,
#' unescaped `#` starts a comment that lasts until the end of the line. A
#' token that does not start with a dash and follows an option is taken as its
#' value (e.g., `--time 01:00:00`).
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

  # Like Slurm, we stop reading at the first line that is neither blank nor a
  # comment (e.g., the first command).
  opts <- character(0)
  for (line in dat) {

    if (grepl("^#SBATCH", line))
      opts <- c(opts, sub("^#SBATCH", "", line))
    else if (!grepl("^\\s*(#.*)?$", line))
      break

  }

  if (!length(opts))
    return(list())

  tokens <- unlist(lapply(opts, split_sbatch_line), use.names = FALSE)

  parse_sbatch_tokens(tokens)

}

#' Splits the arguments of a `#SBATCH` line
#'
#' It follows the rules of the function `get_argument()` in Slurm's
#' `src/sbatch/opt.c`: arguments are separated by white space, quotes group
#' characters (and are removed), a backslash escapes the next character, and an
#' unquoted `#` starts a comment.
#' @param line Character scalar. The line without the leading `#SBATCH`.
#' @return A character vector with the arguments.
#' @noRd
split_sbatch_line <- function(line) {

  chars  <- strsplit(line, "", fixed = TRUE)[[1L]]
  tokens <- character(0)
  cur    <- character(0)
  active <- FALSE # Whether we are building a token (it may be empty, e.g., "")
  quote  <- ""
  i      <- 1L
  n      <- length(chars)

  while (i <= n) {

    ch <- chars[i]

    if (ch == "\\") {

      # Escaped character
      if (i < n) {
        cur    <- c(cur, chars[i + 1L])
        active <- TRUE
      }
      i <- i + 1L

    } else if (nzchar(quote)) {

      if (ch == quote)
        quote <- ""
      else
        cur <- c(cur, ch)

    } else if (ch %in% c("\"", "'")) {

      quote  <- ch
      active <- TRUE

    } else if (ch == "#") {

      break

    } else if (grepl("[[:space:]]", ch)) {

      if (active) {
        tokens <- c(tokens, paste(cur, collapse = ""))
        cur    <- character(0)
        active <- FALSE
      }

    } else {

      cur    <- c(cur, ch)
      active <- TRUE

    }

    i <- i + 1L

  }

  if (nzchar(quote))
    warning(
      "Unmatched `", quote, "` in the #SBATCH line: ", line, call. = FALSE
    )

  if (active)
    tokens <- c(tokens, paste(cur, collapse = ""))

  tokens

}

#' Turns the arguments of the `#SBATCH` lines into a named list
#' @param tokens Character vector as returned by [split_sbatch_line()].
#' @noRd
parse_sbatch_tokens <- function(tokens) {

  ans <- list()
  i   <- 1L
  n   <- length(tokens)

  while (i <= n) {

    tk <- tokens[i]
    i  <- i + 1L

    if (grepl("^--[^-]", tk)) {

      # Long options: --name, --name=value, or --name value. Only the first
      # `=` separates the name from the value (e.g., --export=ALL,FOO=bar)
      eq   <- regexpr("=", tk, fixed = TRUE)[1L]
      name <- if (eq > 0L) substr(tk, 3L, eq - 1L) else substring(tk, 3L)
      val  <- if (eq > 0L) substring(tk, eq + 1L) else NULL

    } else if (grepl("^-[^-]", tk)) {

      # Short options: -n, -n value, -nvalue, or -n=value
      name <- substr(tk, 2L, 2L)
      val  <- substring(tk, 3L)
      val  <- if (nzchar(val)) sub("^=", "", val) else NULL

    } else
      next # Not an option, skip

    # Without an attached value, a following argument that is not an option is
    # the value. Otherwise, this is a flag without a value (e.g., --exclusive)
    if (is.null(val)) {

      if (i <= n && !startsWith(tokens[i], "-")) {
        val <- tokens[i]
        i   <- i + 1L
      } else
        val <- TRUE

    }

    if (nzchar(name))
      ans[length(ans) + 1L] <- list(val)
    if (nzchar(name))
      names(ans)[length(ans)] <- name

  }

  ans

}

#' Job name of a list of `#SBATCH` options
#'
#' The job name can be set using either `--job-name` or `-J`, and the last one
#' wins (as in Slurm). This returns the options with the name stored as
#' `job-name` only.
#' @param opts A named list as returned by [read_sbatch()].
#' @noRd
normalize_job_name <- function(opts) {

  idx <- which(names(opts) %in% c("job-name", "J"))

  if (!length(idx))
    return(opts)

  name <- opts[[idx[length(idx)]]]
  opts <- opts[-idx]

  if (is.character(name))
    opts[["job-name"]] <- name

  opts

}
