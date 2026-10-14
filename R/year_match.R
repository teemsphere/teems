#' @importFrom purrr map_lgl
#' @importFrom data.table set
#' @keywords internal
#' @noRd
.match_year <- function(data,
                        sets,
                        time_steps) {
  int_sets <- sets[purrr::map_lgl(sets, \(s) {
    isTRUE(attr(s, "intertemporal"))
  })]
  stnd_names <- .dock_tail(colnames(data))

  if (any(stnd_names %in% names(int_sets))) {
    TIMEt <- colnames(data)[which(stnd_names %in% names(int_sets))]
    year_cols <- if (length(TIMEt) == 1L) {
      "Year"
    } else {
      paste0("Year_", TIMEt)
    }
    for (i in seq_along(TIMEt)) {
      r_idx <- match(data[[TIMEt[i]]], time_steps$all_time)
      data.table::set(data, j = year_cols[i], value = time_steps$CYRS[r_idx])
    }
  }
  return(data)
}
