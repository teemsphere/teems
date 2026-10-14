#' @title Prepare a scenario shock
#' @export
#' @description Prepares heterogeneous absolute-value trajectory
#'   shocks for intertemporal models. Inputs are aggregated
#'   according to the set mappings in [`ems_data()`] and converted
#'   internally to percentage changes (ordinary changes for a
#'   variable declared `(change)`), making scenario shocks portable
#'   across aggregations. By default every tuple of the variable must be
#'   present in `input` at full pre-aggregation resolution. With
#'   `partial = TRUE`, only some aggregated tuples are shocked (e.g.,
#'   one region's GDP): every pre-aggregation tuple, in every year,
#'   that maps to a shocked aggregated tuple must be present, and an
#'   aggregated tuple covered in part stops the deployment by name.
#'   Values are summed over the pre-aggregation tuples, so scenario
#'   shocks suit additive variables (values and quantities).
#'
#'   See the
#'   \href{https://teemsphere.github.io/scenario_shocks.html}{scenario
#'   shocks} chapter of the user manual for an illustrative example.
#' @return A `list` object with shock configuration to be passed
#'   to the `shock` argument of [`ems_deploy()`].
#' @inheritParams ems_uniform_shock
#' @param input Path to a CSV file, or a data frame or data frame
#'   extension (e.g., tibble, data.table). Must contain a `Value`
#'   column denominated in the actual values of the variable to
#'   be shocked (e.g., million USD) and a `Year` column of
#'   chronological years, plus one column per set index for the
#'   variable. Note that set names must conform to the
#'   model-specific hybrid format (e.g., `ACTSa`, `REGr`). Column
#'   order is inconsequential.
#' @param partial Logical of length 1 (default `FALSE`). If `TRUE`,
#'   `input` may cover a subset of the variable's aggregated tuples;
#'   only those are shocked, and they must be exogenous. Uncovered
#'   exogenous tuples keep a zero change.
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function. [`ems_swap()`] for changing the standard model
#'   closure.
ems_scenario_shock <- function(var,
                               input,
                               partial = FALSE
) {
if (missing(var)) {
  .cli_missing(var)
}
if (missing(input)) {
  .cli_missing(input)
}
args_list <- mget(names(formals()))
call <- match.call()
shock <- .implement_shk(
  args_list = args_list,
  class = "scenario",
  call = call
)
shock
}