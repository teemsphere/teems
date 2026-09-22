#' @keywords internal
#' @noRd
.class_har_headers <- function(headers,
                               data_type,
                               metadata) {
  headers <- lapply(
    headers,
    \(h) {
      header <- h$header
      name <- h$name
      .data <- h$data
      if (!is.null(.data)) {
        if (!toupper(header) %in% c("DREL", "DVER", "XXHS")) {
          .data <- .fold_elements(.data)
        }
        if (is.null(name)) {
          class(.data) <- c(header, data_type, metadata$data_format, class(.data))
        } else {
          class(.data) <- c(header, name, data_type, metadata$data_format, class(.data))
        }
      }
      return(.data)
    }
  )
  return(headers)
}
