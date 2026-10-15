#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
.typed_values <- function(x,
                          type,
                          ndigits) {
  if (type %=% "Integer") {
    typed <- as.integer(x)
    return(typed)
  }
  if (rlang::is_integerish(x)) {
    return(x)
  }
  typed <- .round_digits(x, ndigits)
  return(typed)
}
