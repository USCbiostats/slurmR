
save_objects <- function(
  objects,
  compress = TRUE,
  ...
  ) {

  # Creating and checking  path
  path <- paste(
    options_sluRm$get_job_path(),
    options_sluRm$get_job_name(),
    sep="/"
    )

  # Saving objects
  Map(
    function(n, x) saveRDS(x, n, compress = compress, ...),
    x = objects,
    n = sprintf("%s/%s.rds", path, names(objects))
  )

  names(objects)

}

process_flags <- function(x) {

  # If no flags are passed, then return ""
  if (!length(x))
    return("")

  option <- ifelse(
    nchar(names(x)) > 1,
    paste0("--", names(x)),
    paste0("-", names(x))
  )

  vals <- character(length(option))
  for (i in seq_along(x)) {
    if (is.logical(x[[i]]) && !x[[i]])
      option[i] <- ""
    else if (!is.logical(x[[i]]))
      vals[i] <- paste0("=", x[[i]])
  }

  sprintf("%s%s", option, vals)
}

#' Function to create filenames with full paths for sluRm.
#' @param type can be either of r, sh, out, or rds, and depending on that
#' @noRd
snames <- function(type, array_id) {

  if (!missing(array_id) && length(array_id) > 1)
    return(sapply(array_id, snames, type = type))


  type <- switch (
    type,
    r   = "00-rscript.r",
    sh  = "01-bash.sh",
    out = return("02-output%a.out"),
    rds = sprintf("03-answer-%03i.rds", array_id),
    fin = sprintf("04-finito-%03i.fin", array_id),
    stop(
      "Invalid type, the only valid types are `r`, `sh`, out, and `rds`.",
      call. = FALSE
    )
  )

  sprintf(
    "%s/%s/%s",
    options_sluRm$get_job_path(),
    options_sluRm$get_job_name(),
    type
  )

}

