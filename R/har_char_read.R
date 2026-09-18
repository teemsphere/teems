# 1CFULL: fixed-width character payloads. The history header keeps its
# padding; non-ASCII fields are tagged from the bytes themselves
#' @keywords internal
#' @noRd
.read_har_char <- function(headers) {
  for (h in names(headers)) {
    if (headers[[h]]$type == "1CFULL") {
      contents <- har_payload_concat(
        headers[[h]]$records[3:length(headers[[h]]$records)],
        16L
      )

      # do not remove empty space in the history header. Non-ASCII
      # fields (GTAP 11's LREG among them) are tagged by
      # har_fixed_width_strings from the bytes themselves, so no header
      # here needs an encoding of its own
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
