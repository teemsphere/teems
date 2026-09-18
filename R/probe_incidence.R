#' @description Statement-level equation-system incidence in long form:
#'   one row per (equation statement, referenced variable) with the
#'   element-level incidence weight.
#' @keywords internal
#' @noRd
.probe_incidence <- function(s) {
  if (is.null(s) || !NROW(s)) {
    incidence <- tibble::tibble(
      eq = character(),
      var = character(),
      weight = integer(),
      rows = integer()
    )
    return(incidence)
  }
  pieces <- lapply(seq_len(NROW(s)), \(i) {
    v <- s$vars[[i]]
    if (is.null(v) || !NROW(v)) {
      return(NULL)
    }
    data.table::data.table(
      eq = s$eq[[i]],
      var = v$v,
      weight = as.integer(v$w),
      rows = as.integer(s$rows[[i]])
    )
  })
  incidence <- tibble::as_tibble(data.table::rbindlist(pieces))
  return(incidence)
}
