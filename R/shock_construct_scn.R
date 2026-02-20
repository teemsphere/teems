#' @importFrom data.table fread CJ setnames fsetdiff
#' @importFrom purrr pluck map
#' @importFrom tibble tibble
#' @importFrom rlang abort
#' @importFrom utils capture.output
#' 
#' @noRd
#' @keywords internal
#' @method .construct_shock scenario
#' @export
.construct_shock.scenario <- function(raw_shock,
                                      closure,
                                      sets,
                                      ...) {
  # NSE
  Value <- NULL

  set_ele <- with(sets$ele, mget(raw_shock$ls_upper))
  template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template_shk, new = raw_shock$ls_upper)
  value <- raw_shock$input
  
  class(value) <- c("dat", class(value))
  data.table::setnames(value, raw_shock$ls_mixed, raw_shock$ls_upper)
  value <- .aggregate_data(dt = value,
                           sets = sets$mapping,
                           shock = TRUE)
  
  if (.o_check_shock_status()) {
    if (!data.table::fsetequal(template_shk, value[, !"Value"])) {
      missing_tuples <- data.table::fsetdiff(template_shk, value[, !"Value"])
      n_missing_tuples <- nrow(missing_tuples)
      missing_tuples <- utils::capture.output(print(missing_tuples))[-c(1:3)]
      .cli_action(
        shk_err$scen_missing_tup,
        action = c("abort", "inform"),
        call = attr(raw_shock, "call")
      )
    }
  }

  # some grep should be which
  int_set_names <- sets[sets$qualifier_list == "(intertemporal)", "name"][[1]]
  int_col <- which(colnames(value) %in% int_set_names)
  data.table::setnames(value, raw_shock$ls_upper, raw_shock$ls_mixed)
  non_int_col <- colnames(value)[-c(int_col, ncol(value))]
  int_col <- colnames(value)[int_col]
  
  value[, let(Value = {
    baseline <- Value[get(int_col) == 0]
    (Value - baseline) / baseline * 100
  }), by = non_int_col]


  raw_shock$input <- value
  class(raw_shock) <- "custom"

  .construct_shock(
    raw_shock = raw_shock,
    closure = closure,
    sets = sets
  )
}
