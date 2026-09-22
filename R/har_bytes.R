#' @keywords internal
#' @noRd
.har_bytes <- function(input) {
  if (is.character(input)) {
    cf <- readBin(input, raw(), n = file.size(input))
  } else {
    cf <- raw()
    while (length(a <- readBin(input, raw(), n = 1e9)) > 0) {
      cf <- c(cf, a)
    }
    close(input)
  }
  return(cf)
}
