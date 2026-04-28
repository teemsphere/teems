#' @keywords internal
#' @noRd
#' @method `[` ems_data
#' @export
`[.ems_data` <- function(x, i) {
  attrs <- attributes(x)
  result <- NextMethod()
  attrs$names <- names(result)
  attributes(result) <- attrs
  result
}