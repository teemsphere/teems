#' Prepare a scenario shock
#'
#' Prepares absolute-value trajectory shocks at full
#' pre-aggregation resolution. Inputs are aggregated according to
#' the set mappings in [`ems_data()`] and converted to percentage
#' changes internally, making scenario shocks portable across
#' aggregations. All tuples must be present in `input` — partial
#' variable scenario shocks are not permitted.
#'
#' @inheritParams ems_uniform_shock
#' @param input Path to a CSV file, or a data frame or data frame
#'   extension (e.g., tibble, data.table). Must contain a `Value`
#'   column of absolute values, a `Year` column of chronological
#'   years, and one column per set index for the variable (e.g.,
#'   `REGr`). Column order is inconsequential.
#' @seealso [`ems_deploy()`] for loading the output of this
#'   function. [`ems_swap()`] for changing the standard model
#'   closure.
#' @return A `list` object with shock configuration to be passed
#'   to the `shock` argument of [`ems_deploy()`].
#' @examples
#' \dontrun{
#'  # Chronological time steps
#'  year <- 2023
#'  time_steps <- year + c(0, 1, 2)
#'  
#'  # Input data
#'  dat <- ems_data(
#'    dat_input = "v7_data/gsdfdat.har",
#'    par_input = "v7_data/gsdfpar.har",
#'    set_input = "v7_data/gsdfset.har",
#'    REG = "big3",
#'    ACTS = "macro_sector",
#'    ENDW = "labor_agg",
#'    time_steps = time_steps
#'  )
#'  
#'  # Population data at full resolution
#'  pop <- ems_data(
#'    dat_input = "v7_data/gsdfdat.har",
#'    par_input = "v7_data/gsdfpar.har",
#'    set_input = "v7_data/gsdfset.har",
#'    REG = "full",
#'    ACTS = "macro_sector",
#'    ENDW = "labor_agg",
#'  )$POP
#'  
#'  # Create trajectories
#'  pop$Year <- year
#'  regions <- unique(pop$REG)
#'  pop_traj <- expand.grid(
#'    REG = regions,
#'    Value = 0,
#'    Year = tail(time_steps, -1),
#'    stringsAsFactors = FALSE
#'  )
#'  
#'  pop <- rbind(pop, pop_traj)
#'  growth_rates <- data.frame(
#'    REG = regions,
#'    growth_rate = runif(length(regions), min = -0.01, max = 0.05)
#'  )
#'  
#'  pop <- merge(pop, growth_rates, by = "REG")
#'  base_values <- pop[pop$Year == year, c("REG", "Value")]
#'  names(base_values)[2] <- "base_value"
#'  pop <- merge(pop, base_values, by = "REG")
#'  
#'  pop$Value[pop$Year > year] <-
#'    pop$base_value[pop$Year > year] *
#'    (1 + pop$growth_rate[pop$Year > year])^(pop$Year[pop$Year > year] - year)
#'  
#'  pop$growth_rate <- NULL
#'  pop$base_value <- NULL
#'  pop <- pop[order(pop$REG, pop$Year), ]
#'  pop <- pop[, c("REG", "Year", "Value")]
#'  colnames(pop)[1] <- "REGr"
#'  
#'  pop_trajectory <- ems_scenario_shock(
#'    var = "pop",
#'    input = pop
#'  )
#' }
#' @export
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