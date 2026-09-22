#' @keywords internal
#' @noRd
.match_bracket <- function(s, i) {
  scan <- .tab_scan(s)
  target <- scan$depth_before[i] + 1L
  n <- length(scan$chs)
  j <- i + 1L
  while (j <= n) {
    if (scan$depth_before[j] == target &&
      scan$chs[j] %in% c(")", "]", "}") && !scan$in_quote[j]) {
      return(j)
    }
    j <- j + 1L
  }
  return(NA_integer_)
}
