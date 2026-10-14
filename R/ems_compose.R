#' @title Compose model results into structured data objects
#' @export
#' @description `ems_compose()` retrieves and processes results
#'   from a solved model run. Data validation and consistency
#'   checks are performed during composition.
#' @return A tibble with columns "name", "label", "type", and
#'   "dat" (a list-column of data.tables) containing model
#'   results. For runs solved with an embedded Runge-Kutta method
#'   (`"BoSha32"`, `"DoPri54"`; see [`ems_solve()`]) each
#'   variable's data.table carries an additional `error_estimate`
#'   column: the solver's component-by-component estimate of the
#'   cumulative solution error, `|delta| / max(1, |Value|)`.
#'   Sorting on `error_estimate` ranks the results by how settled
#'   they are; the estimate is an indicator, not a bound on the
#'   error.
#' @inheritParams ems_solve
#' @param which Character vector of variable length (default
#'   `"all"`). When `"all"`, all model variables and coefficients
#'   (updated, post-simulation values, plus any PostSim
#'   coefficients typed `"postsim"`) are returned. Condensation
#'   coefficients (`CSUB<n>`) are returned only when named.
#'   Otherwise, a character vector of variable and/or coefficient
#'   names to retrieve (case insensitive). Note that some
#'   coefficient names differ from their associated headers
#'   (e.g., VTWR/VTMFSD). Coefficient values are read from the
#'   solver's binary coefficient dump (`sol.cof`/`sol.cbin`).
#' @param passes Logical length 1 (default `FALSE`). When `TRUE`,
#'   each variable's data.table gains the three separate
#'   solutions of a `"Gragg"`, `"Midpoint"` or `"Euler"` run and
#'   their accuracy columns, and the output gains an `accuracy`
#'   attribute (see Details). An error is raised for a run that
#'   wrote no separate solutions.
#' @details Every variable whose pre-simulation level is known
#'   gains the columns `pre_level` and `post_level`, plus
#'   `change` (the ordinary change) for a percent-change variable
#'   or `percent_change` for a change variable. The level is
#'   known for a variable declared with `ORIG_LEVEL=` (a
#'   coefficient over exactly the variable's sets, or a number)
#'   and for a levels variable; pre-simulation values come from
#'   the solver's `sol.cbin0`.
#'
#'   With `passes = TRUE`, each variable's data.table gains, in
#'   the units of `Value` where applicable:
#'   * `pass1` to `pass3`: the separate solutions, fewest to most
#'   steps.
#'   * `figures`: the significant figures on which the two
#'   extrapolations agree (0 to 6, `NA` when both are below
#'   1e-6); `levels_figures`: the same for the levels value.
#'   * `error`: the difference between the two extrapolations.
#'   * `code`: the convergence code, best `"XMA"` to worst
#'   `"OD!"`; see GEMPACK manual 26.2.3.
#'   * `summarised`: whether the element counts in the run's
#'     accuracy summary: `TRUE` for endogenous and backsolved
#'     results (of the `accuracy_vars` variables when those are
#'     given), `FALSE` for exogenous elements, whose values the
#'     shock sets.
#'
#'   With subintervals, `figures` and `levels_figures` are the
#'   lowest over the subintervals and `code` the worst; `pass1`
#'   to `pass3` and `error` come from the last subinterval alone,
#'   each pass compounded onto the extrapolated result of the
#'   subintervals before it. The per-variable summary,
#'   `attr(output, "accuracy")`, has `elements`, `summarised`,
#'   `min_figures`, `min_levels_figures` (a zero result counts as
#'   6), `share_figures`, `share_levels_figures` (share at
#'   `accuracy_figures` or more, see [`ems_option_set()`]),
#'   `worst_code` and `max_error`.
#' @seealso [`ems_solve()`] for solving the CGE model.
#' @examples
#' \dontrun{
#' # The following examples require that a model run has taken
#' # place. See https://teemsphere.github.io/ to get started.
#'
#' # Return all variables and coefficients
#' outputs <- ems_compose(cmf_path)
#'
#' # Return specific variables and/or coefficients by name
#' outputs <- ems_compose(cmf_path, c("qfd", "EVFP"))
#'
#' # Add the three separate solutions of a Gragg run
#' outputs <- ems_compose(cmf_path, "qgdp", passes = TRUE)
#' }
ems_compose <- function(cmf_path,
                        which = "all",
                        passes = FALSE
) {
if (missing(cmf_path)) {
  .cli_missing(cmf_path)
}
args_list <- mget(names(formals()))
call <- match.call()
output <- .implement_compose(
  args_list = args_list,
  call = call
)
output
}
