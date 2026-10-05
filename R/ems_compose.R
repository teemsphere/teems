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
#' @inheritParams ems_solve
#' @param which Character vector of variable length (default
#'   `"all"`). When `"all"`, all model variables and all model
#'   coefficients (updated, post-simulation values, plus any
#'   PostSim coefficients typed `"postsim"`) are returned.
#'   Otherwise, a character vector of variable and/or coefficient
#'   names to retrieve — an error is raised for any name not
#'   found. Note that some coefficient names differ from their
#'   associated headers (e.g., VTWR/VTMFSD). Coefficient values
#'   are read from the solver's binary coefficient dump
#'   (`sol.cof`/`sol.cbin`), selectively for named coefficients;
#'   the per-coefficient CSV files (see `write_coefficients` in
#'   [`ems_deploy()`]) are used only when the dump is absent.
#' @param passes Logical length 1 (default `FALSE`). When `TRUE`,
#'   each variable's data.table gains the separate solutions that a
#'   three-pass `"Gragg"`, `"Midpoint"` or `"Euler"` run extrapolates
#'   from (GEMPACK's SSL, manual 26.8.1): `Pass1`, `Pass2` and `Pass3`
#'   for the solutions with the fewest to the most steps, in the
#'   units of `Value`, and `Accuracy`, the number of significant
#'   figures to which the passes agree (6 for six or more, 1 for one
#'   or none; manual 26.2.3). With subintervals each pass is that
#'   pass over the last subinterval, compounded onto the
#'   extrapolated result of the earlier ones. An error is raised for
#'   a run that wrote no separate solutions.
#' @details Every variable whose pre-simulation level is known
#'   gains the columns `PreLevel` and `PostLevel`, plus `Change` (the ordinary
#'   change) for a percent-change variable or `PercentChange` for a
#'   change variable (GEMPACK levels results, manual 11.6.5). The
#'   level is known for a variable declared with `ORIG_LEVEL=` (a
#'   coefficient over exactly the variable's sets, or a number) and
#'   for a levels variable; pre-simulation values come from the
#'   solver's `sol.cbin0`.
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
