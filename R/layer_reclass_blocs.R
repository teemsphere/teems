#' BLOC and the REGTOBLOC mapping are the only character headers read
#' from GTAPPARM rather than GTAPSETS, so the loader classes them as
#' parameters and they would reach the numeric aggregation path. TRBL
#' carries the bloc elements, which the database ships one-to-one with
#' REG, so it follows that set's aggregation; MAPB is the mapping's
#' region list at source resolution, which .finalize_map_data composes
#' onto the aggregated BLOC at deploy, so it stays a user set.
#'
#' @keywords internal
#' @noRd
.layer_reclass_blocs <- function(i_data, fmt) {
  nm <- toupper(names(i_data))
  for (header in c("TRBL", "MAPB")) {
    i <- match(header, nm)
    if (is.na(i)) {
      next
    }
    i_data[[i]] <- .layer_set(
      header, tolower(trimws(as.character(i_data[[i]]))), fmt,
      user_set = header %=% "MAPB"
    )
  }
  return(i_data)
}
