#' @keywords internal
#' @noRd
.round_digits <- function(x, ndigits) {
  digits <- ndigits - 1 - floor(log10(abs(x)))
  digits[!is.finite(digits) | digits < ndigits] <- ndigits
  rounded <- round(x, digits)
  return(rounded)
}
