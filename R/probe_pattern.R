#' @keywords internal
#' @noRd
.probe_pattern <- function(p,
                           n) {
  if (is.null(p)) {
    return(NULL)
  }
  pattern <- list(
    flag = p$flag,
    entries = p$entries %|||% NA_integer_,
    n = n,
    rank = p$rank %|||% NA_integer_,
    unmatched_rows = p$unmatched_rows %|||% NA_integer_,
    unmatched_cols = p$unmatched_cols %|||% NA_integer_,
    defective = isTRUE(p$defective),
    under_determined = .probe_element_tbl(p$under_determined_vars),
    over_constrained = .probe_element_tbl(p$over_constrained_eqs),
    under_by_var = .probe_agg_tbl(p$under_determined_by_var),
    over_by_eq = .probe_agg_tbl(p$over_constrained_by_eq),
    dm = p$dm,
    dm_under_by_var = .probe_agg_tbl(p$dm_under_by_var),
    dm_over_by_eq = .probe_agg_tbl(p$dm_over_by_eq)
  )
  return(pattern)
}
