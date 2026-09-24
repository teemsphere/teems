#' @importFrom data.table set
#' @keywords internal
#' @noRd
.order_by_sets <- function(dt,
                           dt_sets) {
  dims <- setdiff(colnames(dt), "Value")
  for (j in seq_along(dims)) {
    ele <- dt_sets[[j]]
    col <- dt[[dims[j]]]
    if (is.factor(col) || !all(col %in% ele)) {
      next
    }
    data.table::set(dt, j = dims[j], value = factor(col, levels = unique(ele)))
  }
  return(invisible(dt))
}
