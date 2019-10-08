#' @export
#' @param simplify Logical scalar. See [sapply].
#' @details Just like [sapply] is to [lapply], `Slurm_sapply` is just a wrapper of
#' `Slurm_lapply` with an extra argument, `simplify`. When `TRUE`, once the job
#' is collected, the function [simplify2array] is called.
#' @rdname Slurm_lapply
Slurm_sapply <- function(
  X,
  FUN,
  ...,
  simplify = TRUE
) {

  dots <- list(...)
 
  if (is.null(dots$export_env))
    dots$export_env <- parent.frame()
  
  if (simplify)
    dots$hooks <- c(dots$hooks, list(simplify2array))

  do.call(
    "Slurm_lapply",
    c(list(X = X, FUN = FUN), dots)
  )

}
