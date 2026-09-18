# 2RFULL: dense real arrays
#' @keywords internal
#' @noRd
.read_har_real <- function(headers) {
  for (h in names(headers)) {
    if (headers[[h]]$type == "2RFULL") {
      m <- array(
        har_payload_f32(
          headers[[h]]$records[3:length(headers[[h]]$records)],
          32L,
          prod(headers[[h]]$dimensions)
        ),
        dim = headers[[h]]$dimensions
      )
      headers[[h]]$data <- m
    }
  }
  return(headers)
}
