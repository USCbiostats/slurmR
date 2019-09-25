#' Expand Array Indexes
#'
#' When submitting array jobs using `sbatch`, users can specify ranges in several
#' ways. These could be specified as ranges, "`1-9`", lists, "`1,2,5`", or intervals
#' as "`1-7:3`", which translates into "`1, 4, 7`". This function expands those
#' cases.
#'
#' @param x A character vector. Array indexes (see details).
#'
#' @details
#' `x` is assumed to be in the form of `[jobid]_[array expression]`. The first
#' The function will return an expanded version of this, e.g. if `x = "8123_[1,3-6]"`
#' the resulting expression will be the vector  "8123_1", "8123_3", "8123_4",
#' "8123_5", and "8123_6".
#'
#' This function was developed mainly to be used internally.
#' @return A character vector with the expanded indices.
#' @examples
#'
#' expand_array_indexes(c("123_1", "55_[1-5]", "122_[1, 5-6]", "44_[1-3:2]"))
#' @export
expand_array_indexes <- function(x) {

  if (length(x) > 1)
    return(
      unname(
        unlist(sapply(x, expand_array_indexes, simplify = FALSE), recursive = TRUE)
      )
    )

  # is it simply a number?
  if (grepl("^[0-9]+$", x))
    return(as.integer(x))

  # Capturing main job name
  jobid <- as.integer(gsub("_.+", "", x))
  x     <- gsub(".+_", "", x)

  # Removing possible values of "simulatenous jobs"
  x <- gsub("%[0-9]+", "", x)

  # Simplest case
  if (grepl("^[0-9]+$", x))
    return(as.integer(x))

  # Removing the brackets
  x <- gsub("^\\[|\\]$", "", x)

  # Splitting (if any)
  x <- strsplit(x, "\\s*[,]\\s*")[[1L]]

  # Now, checking for each component
  ans <- NULL
  for (i in x) {

    # Case 0: Most complicated, a range
    if (grepl("[:]", i)) {

      step. <- as.integer(gsub(".+[:]", "", i))
      start <- as.integer(gsub("[-].+", "", i))
      end   <- as.integer(gsub("^[0-9]+[-]|[:].+", "", i))
      ans <- c(ans, seq(start, end, by = step.))

    } else if (grepl("^[0-9]+[-][0-9]+$", i)) { # Case 1: A range

      start <- as.integer(gsub("[-].+", "", i))
      end   <- as.integer(gsub(".+[-]", "", i))
      ans <- c(ans, start:end)

    } else if (grepl("^[0-9]+$", i)) { #A simple number!
      ans <- c(ans, as.integer(i))
    } else
      stop("Unknown expression for a Job Array", call. = FALSE)

  }

  return(paste0(jobid, "_", ans))


}
