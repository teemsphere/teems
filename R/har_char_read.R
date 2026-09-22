#' @keywords internal
#' @noRd
.read_har_char <- function(headers) {
  for (h in names(headers)) {
    if (headers[[h]]$type == "1CFULL") {
      contents <- har_payload_concat(
        headers[[h]]$records[3:length(headers[[h]]$records)],
        16L
      )

      trim <- tolower(h) != "xxhs"
      toRet <- har_fixed_width_strings(
        contents,
        headers[[h]]$dimensions[[2]],
        trim
      )

      headers[[h]]$data <- toRet
    }
  }
  return(headers)
}
