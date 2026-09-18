#' @importFrom purrr pluck
#'
# the release and version headers turned into the run's metadata
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
    # the release letter and any model-layer suffix are dropped to leave
    # the version number the mappings and set conversions are keyed on:
    # GTAPv12a and GTAPv12aPower both key on GTAPv12. Anchoring the strip
    # matters, a trailing pattern having eaten the P of Power and left
    # GTAPv12aower, which matches no mapping bucket at all.
    metadata[["database_version"]] <- sub(
      "^(GTAP[A-Za-z]*[0-9]+).*$", "\\1",
      metadata[["database_version"]]
    )
  return(metadata)
}
