# the whole HAR as raw bytes, from a path or an open connection
#' @keywords internal
#' @noRd
.har_bytes <- function(input) {
  if (is.character(input)) {
    cf <- readBin(input, raw(), n = file.size(input))
  } else {
    # Read all bytes into a vector
    cf <- raw()
    while (length(a <- readBin(input, raw(), n = 1e9)) > 0) {
      cf <- c(cf, a)
    }
    close(input)
  }
  return(cf)
}
