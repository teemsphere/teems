#' @keywords internal
#' @noRd
.tab_number <- function(x) {
  vapply(as.numeric(x), \(v) format(v, scientific = FALSE, digits = 15L, trim = TRUE), character(1L))
}
