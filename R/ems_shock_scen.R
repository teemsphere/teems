#' @title Prepare a scenario shock
#' @export
#' @description Prepares heterogeneous absolute-value trajectory
#'   shocks. Inputs are aggregated according to the set mappings
#'   in [`ems_data()`] and converted to percentage changes
#'   internally, making scenario shocks portable across
#'   aggregations. All tuples must be present in `input` at full
#'   pre-aggregation resolution. Partial variable scenario shocks
#'   are not permitted.
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
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function. [`ems_swap()`] for changing the standard model
#'   closure.
ems_scenario_shock <- function(var,
                               input
) {
if (missing(var)) {
  .cli_missing(var)
}
if (missing(input)) {
  .cli_missing(input)
}
args_list <- mget(names(formals()))
call <- match.call()
shock <- .implement_shock(
  args_list = args_list,
  class = "scenario",
  call = call
)
shock
}