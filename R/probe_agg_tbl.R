#' @keywords internal
#' @noRd
.probe_agg_tbl <- function(x) {
  if (is.null(x) || !NROW(x)) {
    tbl <- tibble::tibble(name = character(), count = integer())
    return(tbl)
  }
  tbl <- tibble::tibble(name = x$n, count = as.integer(x$c))
  return(tbl)
}
