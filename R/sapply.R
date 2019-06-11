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

  Slurm_lapply(X, FUN, ..., hooks = if (simplify)
    list(simplify2array) else NULL)

}
