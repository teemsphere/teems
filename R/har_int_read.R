#' @keywords internal
#' @noRd
.read_har_int <- function(headers) {
  for (h in names(headers)) {
    if (headers[[h]]$type == "2IFULL") {
      m <- matrix(
        har_payload_i32(
          headers[[h]]$records[3:length(headers[[h]]$records)],
          32L,
          prod(headers[[h]]$dimensions)
        ),
        nrow =
          headers[[h]]$dimensions[[1]],
        ncol =
          headers[[h]]$dimensions[[2]]
      )
      headers[[h]]$data <- m
    }
  }
  return(headers)
}
