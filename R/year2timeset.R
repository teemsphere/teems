#' @importFrom purrr pluck
#' @importFrom data.table setnames
#' @importFrom utils capture.output
#' 
#' @noRd
#' @keywords internal
.year2time_set <- function(shk,
                           sets,
                           int_set_names) {
  time_set_upper <- intersect(shk$ls_upper, int_set_names)
  time_set <- shk$ls_mixed[match(time_set_upper, shk$ls_upper)]
  CYRS <- attr(sets, "CYRS")

  if (!all(shk$input$Year %in% CYRS$Value)) {
    errant_year_tuples <- shk$input[!shk$input$Year %in% CYRS$Value]
    n_errant_year_tuples <- nrow(errant_year_tuples)
    errant_year_tuples <- utils::capture.output(print(errant_year_tuples))[-c(1, 2)]
    .cli_action(
      shk_err$invalid_year,
      action = "abort",
      call = attr(shk, "call")
    )
  }
  time_set_ele <- purrr::pluck(sets, "ele", time_set_upper)
  r_idx <- match(shk$input$Year, CYRS$Value)
  shk$input$Year <- CYRS$all_time[r_idx]
  data.table::setnames(shk$input, "Year", time_set)
  shk$set[match("Year", shk$set)] <- time_set
  return(shk)
}