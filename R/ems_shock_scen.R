#' @title Prepare a scenario shock
#' @export
#' @description Prepares a trajectory shock for an intertemporal
#'   model from absolute values: a value per pre-aggregation tuple
#'   and year, in the units of the variable's data (million USD,
#'   persons). At deployment the values are summed over the
#'   aggregation of [`ems_data()`], and each year's aggregated value
#'   becomes the percentage change from the base-year value (the
#'   ordinary change for a variable declared `(change)`). The same
#'   input therefore serves any aggregation. Because the values
#'   are summed, scenario shocks suit additive variables, values
#'   and quantities.
#'
#'   By default `input` must hold every pre-aggregation tuple of
#'   the variable in every year. With `partial = TRUE` it may hold
#'   only the tuples behind some aggregated tuples, each complete:
#'   an aggregated tuple covered in part stops the deployment by
#'   name, and the aggregated tuples not covered keep a zero
#'   change. See the
#'   \href{https://teemsphere.github.io/scenario_shocks.html}{scenario
#'   shocks} chapter of the manual for a worked population
#'   trajectory.
#' @return A `list` object with shock configuration to be passed
#'   to the `shock` argument of [`ems_deploy()`].
#' @inheritParams ems_uniform_shock
#' @param input Path to a CSV file, or a data frame or data frame
#'   extension (e.g., tibble, data.table), with a `Value` column
#'   of absolute values, a `Year` column of chronological years
#'   including the base year, and one column per remaining set
#'   index of the variable in the model-specific format (e.g.,
#'   `REGr`). Column order is inconsequential.
#' @param partial Logical of length 1 (default `FALSE`). If
#'   `TRUE`, `input` may cover a subset of the variable's
#'   aggregated tuples; only those are shocked, and they must be
#'   exogenous.
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function. [`ems_custom_shock()`] for shocks given directly as
#'   percentage or ordinary changes at the model's aggregation.
#' @examples
#' # A trajectory for part of the world: three pre-aggregation
#' # regions that the aggregation of ems_data() groups into one
#' # region, in millions, base year 2017 first. At deployment the
#' # three are summed and each later year becomes the percentage
#' # change from 2017. Without partial = TRUE every region of the
#' # database would have to be present; the manual's population
#' # chapter builds such a trajectory from the POP header.
#' pop <- expand.grid(REGr = c("aus", "nzl", "xoc"),
#'                    Year = 2017:2020,
#'                    stringsAsFactors = FALSE)
#' pop$Value <- c(24.6, 4.8, 1.9) * 1.015^(pop$Year - 2017)
#' pop
#'
#' ems_scenario_shock("pop", pop, partial = TRUE)
#'
#' # The same from a CSV file
#' csv <- tempfile(fileext = ".csv")
#' write.csv(pop, csv, row.names = FALSE)
#' ems_scenario_shock("pop", csv, partial = TRUE)
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