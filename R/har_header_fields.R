# the fields every header carries in its first two records: the
# four-character name, the storage type and the dimensions
#' @keywords internal
#' @noRd
.har_header_fields <- function(headers) {
  for (h in names(headers)) {
    headers[[h]]$header <- trimws(rawToChar(headers[[h]]$records[[1]][1:4]))
    headers[[h]]$type <- rawToChar(headers[[h]]$records[[2]][5:10])
    # headers[[h]]$label <- trimws(rawToChar(headers[[h]]$records[[2]][11:80]))
    headers[[h]]$numberOfDimensions <- readBin(headers[[h]]$records[[2]][81:84], "integer",
      size =
        4
    )

    headers[[h]]$dimensions <- c()

    for (i in 1:headers[[h]]$numberOfDimensions) {
      headers[[h]]$dimensions <- c(
        headers[[h]]$dimensions,
        readBin(headers[[h]]$records[[2]][(85 + (i - 1) * 4):(85 + i * 4)], "integer",
          size =
            4
        )
      )
    }
  }
  return(headers)
}
