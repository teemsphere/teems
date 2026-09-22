#' @keywords internal
#' @noRd
.read_input <- function(input,
                        data_type,
                        metadata = NULL,
                        attach_metadata = FALSE,
                        call = NULL) {
  return(UseMethod(".read_input"))
}

#' @importFrom utils read.csv
#' @method .read_input csv
#' @export
#' @keywords internal
#' @noRd
.read_input.csv <- function(input,
                            data_type,
                            metadata = NULL,
                            attach_metadata = FALSE,
                            call = NULL) {
  input <- utils::read.csv(input)
  if (data_type %=% "set") {
    input <- input[[1]]
  }

  return(input)
}

#' @method .read_input har
#' @export
#' @keywords internal
#' @noRd
.read_input.har <- function(input,
                            data_type,
                            metadata = NULL,
                            attach_metadata = FALSE,
                            call = NULL) {
  cf <- .har_bytes(input)

  headers <- .split_har_headers(cf)

  headers <- .har_header_fields(headers)
  headers <- .read_har_char(headers)
  headers <- .read_har_int(headers)
  headers <- .read_har_real(headers)
  headers <- .read_har_sparse(headers)

  if (attach_metadata) {
    metadata <- .har_metadata(headers = headers, data_type = data_type)
  }

  if (data_type %=% "set") {
    headers <- .har_set_names(headers = headers, metadata = metadata)
  }

  headers <- .class_har_headers(
    headers = headers,
    data_type = data_type,
    metadata = metadata
  )

  class(headers) <- c(data_type, class(headers))
  if (attach_metadata) {
    attr(headers, "metadata") <- metadata
  }

  return(headers)
}

#' @keywords internal
#' @noRd
.fold_elements <- function(x) {
  if (is.character(x)) {
    x[] <- tolower(x)
  }
  if (!is.null(dimnames(x))) {
    dimnames(x) <- lapply(dimnames(x), tolower)
  }
  return(x)
}
