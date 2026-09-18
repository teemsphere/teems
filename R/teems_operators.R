#' @noRd
#' @keywords internal
`%=%` <- function(x, y) {
  same <- identical(x, y)
  return(same)
}

#' @noRd
#' @keywords internal
`%!=%` <- function(x, y) {
  return(!identical(x, y))
}

#' @noRd
#' @keywords internal
`%|||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  return(x)
}