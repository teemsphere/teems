#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
.typed_values <- function(x,
                          type,
                          ndigits) {
  if (type %=% "Integer") {
    return(as.integer(x))
  }
  if (rlang::is_integerish(x)) {
    return(x)
  }
  return(.round_digits(x, ndigits))
}
