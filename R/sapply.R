#' @export
#' @param simplify Logical scalar. See [sapply].
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
