#' Read a slurm batch file and capture the SBATCH options
#'
#' @param x Character scalar. Either the path to the batch file to process, or
#' a character vector.
#' @return A named vector of the options starting with `#SBATCH` in the file.
#' If no option is found, then returns a character vector length 0.
#' @examples
#'
#' # Reading in an example script
#' x <- system.file("example.slurm", package="slurrm")
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
    return(character(0))

  opts <- dat[opts_ids]

  opts <- gsub("^#SBATCH\\s+", "", opts)
  opts_names <- gsub("=.+", "", opts)
  opts_names <- gsub("^[-]{2}", "", opts_names)
  opts       <- gsub(".+=", "", opts)


  structure(
    opts, names = opts_names
  )
}
