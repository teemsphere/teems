#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
.har_metadata <- function(headers,
                          data_type) {
    DREL <- purrr::pluck(headers, "DREL", "data")
    DVER <- purrr::pluck(headers, "DVER", "data")
    metadata <- .har_meta(
      DREL = DREL,
      DVER = DVER,
      data_type = data_type
    )

    metadata[["full_database_version"]] <- metadata[["database_version"]]
    metadata[["database_version"]] <- sub(
      "^(GTAP[A-Za-z]*[0-9]+).*$", "\\1",
      metadata[["database_version"]]
    )
  return(metadata)
}
