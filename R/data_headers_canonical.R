#' @keywords internal
#' @noRd
.canonical_headers <- function(.data,
                               headers) {
  hit <- match(toupper(names(.data)), toupper(headers))
  swap <- which(!is.na(hit) & names(.data) != headers[hit])
  for (i in swap) {
    hdr <- headers[hit[i]]
    cls <- class(.data[[i]])
    cls[1] <- hdr
    if (length(cls) > 1L && cls[2] %=% names(.data)[i]) {
      cls[2] <- hdr
    }
    class(.data[[i]]) <- cls
    names(.data)[i] <- hdr
  }
  return(.data)
}
