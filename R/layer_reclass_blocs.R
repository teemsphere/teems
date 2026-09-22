#' @keywords internal
#' @noRd
.layer_reclass_blocs <- function(i_data, spec, fmt) {
  nm <- toupper(names(i_data))
  for (header in names(spec$reclass)) {
    i <- match(header, nm)
    if (is.na(i)) {
      next
    }
    i_data[[i]] <- .layer_set(
      header, tolower(trimws(as.character(i_data[[i]]))), fmt,
      user_set = spec$reclass[[header]]
    )
  }
  return(i_data)
}
