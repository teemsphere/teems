#' @noRd
#' @keywords internal
`%=%` <- function(x, y) {
  identical(x, y)
}

#' @noRd
#' @keywords internal
`%!=%` <- function(x, y) {
  !identical(x, y)
}

#' @noRd
#' @keywords internal
`%|||%` <- function(x, y) {
  if (is.null(x)) y else x
}