#' Check for possible actions for a `slurm_job` wrapper
#'
#' Users can choose whether to submit the job or not, to wait for it, and whether
#' they want to collect the results right away after the job has finished. This
#' function will help developers to figure out what set of actions need to be
#' taken depending on the plan.
#'
#' @param plan A character scalar with either of the following values:
#' `"collect"`, `"wait"`, `"submit"`, or `"none"`.
#' @details
#' This is a helper function that returns a list with three logical values:
#' `wait`, `collect`, and `submit`. There are four possible cases:
#'
#' - `plan == "collect"`, then all three are `TRUE`.
#' - `plan == "wait"`, then all but `collect` are `TRUE`.
#' - `plan == "submit"` then only `submit` equals `TRUE`.
#' - `plan == "none"` then all three are `FALSE`.
#'
#' In general, bot `wait` and `submit` will be passed to [sbatch].
#'
#' When `collect == TRUE`, then it usually means that the function will be calling
#' [Slurm_collect] right after submitting the job via [sbatch].
#'
#' @return A list with three logical scalars.
#' @seealso This is used in [apply functions][Slurm_lapply] and in [Slurm_EvalQ].
#'
#' @examples
#' the_plan("none")
#' # $collect
#' # [1] FALSE
#' #
#' # $wait
#' # [1] FALSE
#' #
#' # $submit
#' # [1] FALSE
#'
#' the_plan("wait")
#' # $collect
#' # [1] FALSE
#' #
#' # $wait
#' # [1] TRUE
#' #
#' # $submit
#' # [1] TRUE
#' @export
the_plan <- function(plan) {

  if (length(plan) > 1L)
    stop("The `plan` must be a character of length 1.", call. = FALSE)
  if (!is.character(plan))
    stop("The `plan` must be a character of length 1.", call. = FALSE)

  PLAN <- c("collect", "wait", "submit", "none")
  if (!(plan %in% PLAN))
    stop("The `plan` must be either of: \"",
         paste0(PLAN, collapse = "\", \""), "\". Right now is: \"",
         plan, "\".", call. = FALSE)

  res <- list()
  if (plan == "collect") {

    res$collect <- TRUE
    res$wait    <- TRUE
    res$submit  <- TRUE

  } else if (plan == "wait") {

    res$collect <- FALSE
    res$wait    <- TRUE
    res$submit  <- TRUE

  } else if (plan == "submit") {

    res$collect <- FALSE
    res$wait    <- FALSE
    res$submit  <- TRUE

  } else if (plan == "none") {

    res$collect <- FALSE
    res$wait    <- FALSE
    res$submit  <- FALSE

  }

  return(res)

}
