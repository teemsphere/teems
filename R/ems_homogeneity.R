#' @importFrom rlang arg_match
#' @title Check a model's homogeneity
#' @export
#' @description Tests a deployed model for nominal or real
#'   homogeneity from the VPQ types its model file declares (see
#'   [`ems_model()`]; GEMPACK manual chapter 57). The check takes the
#'   linearized system at the base data and evaluates every equation
#'   at the change homogeneity implies: 1 per cent for `Value` and
#'   `Price` variables in a nominal test, for `Value` and `Quantity`
#'   variables in a real one, 0 for the rest. An equation that does not
#'   sum to zero is not homogeneous. No solve is needed, and the
#'   result names the offending equations.
#' @details The error metric of an equation element is
#'   `min(|sum|, |sum| / sum|terms|)` over its terms, and an equation
#'   block reports its maximum (GEMPACK's MaxErrMet); values above
#'   about `1e-6` point at a problem. An element is tested only when
#'   every variable in it has a type other than `Unspecified`; a
#'   change variable also needs its `ORIG_LEVEL` to be typed. Zero
#'   flows can make an equation fail harmlessly (manual 57.2.5). The
#'   check works on the system as deployed, so a condensed model's
#'   equations carry the substituted ones (manual 57.4.5).
#'
#'   With `simulate = TRUE`, the deployment is also solved (Johansen)
#'   with every exogenous variable shocked by the change its type
#'   implies, and each typed variable's result is compared with the
#'   expected one; a variable's error metric is
#'   `|V - E| / max(1, min(|V|, |E|))` (manual 57.5). Both runs work
#'   on a copy of the deployment, so its own results are untouched.
#' @param cmf_path Character length 1, path to the CMF file of a
#'   deployment from [`ems_deploy()`].
#' @param type Character length 1, `"nominal"` (default) or `"real"`.
#' @param simulate Logical length 1 (default `FALSE`). If `TRUE`,
#'   also run the homogeneity simulation.
#' @param ... Arguments passed to [`ems_solve()`] for the simulation
#'   (e.g. `matrix_method`, `n_tasks`); its `solution_method` is
#'   always `"Johansen"`.
#' @return A list with `equations`, one row per equation block
#'   (`tested` is `FALSE` when some element could not be evaluated),
#'   `elements`, one row per equation element, and with
#'   `simulate = TRUE`, `variables`, one row per typed variable.
#' @examples
#' \dontrun{
#' check <- ems_homogeneity(cmf_path, type = "nominal")
#' check$equations
#' }
ems_homogeneity <- function(cmf_path,
                            type = c("nominal", "real"),
                            simulate = FALSE,
                            ...) {
if (missing(cmf_path)) {
  .cli_missing(cmf_path)
}
call <- match.call()
type <- rlang::arg_match(type, error_call = call)
if (!is.logical(simulate) || length(simulate) != 1L || is.na(simulate)) {
  bad_arg <- "simulate"
  .cli_action(gen_err$logical_flag,
    action = "abort",
    call = call
  )
}
output <- .implement_homogeneity(
  cmf_path = .check_input(file = cmf_path, valid_ext = "cmf", call = call),
  type = type,
  simulate = simulate,
  solve_args = list(...),
  call = call
)
output
}
