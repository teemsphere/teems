# the data of every header, lowercased and classed by header, name,
# data type and database format
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
        # element names are lowercase throughout TEEMS: every string
        # header and every dimension label is folded here, once, so the
        # mixed case a database ships (Land, NatlRes, CGDS, AEZ1, and
        # the GTAPv7 ENDW/ENDWS disagreement inside one file) never
        # reaches a mapping check, a set table or a solver file. The
        # release, version and history headers keep their text
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
