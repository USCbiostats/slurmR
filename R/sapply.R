#' @export
#' @param simplify,USE.NAMES Logical scalar. See [sapply].
#' @details Just like [sapply] is to [lapply], `Slurm_sapply` is just a wrapper of
#' `Slurm_lapply` with an extra argument, `simplify`. When `TRUE`, once the job
#' is collected, the function [simplify2array] is called.
#' @rdname Slurm_lapply
Slurm_sapply <- function(
  X,
  FUN,
  ...,
  simplify  = TRUE,
  USE.NAMES = TRUE
) {

  dots <- list(...)

  if (is.null(dots$export_env))
    dots$export_env <- parent.frame()

  if (simplify)
    dots$hooks <- c(dots$hooks, list(simplify2array))

  if (USE.NAMES && is.character(X)) {
    dots$hooks <- c(dots$hooks, function(x) {
      if (is.null(names(x)))
        names(x) <- X
      x
    })
  }


  do.call(
    "Slurm_lapply",
    c(list(X = X, FUN = FUN), dots)
  )

}
