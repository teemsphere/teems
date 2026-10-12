#' @keywords internal
#' @noRd
.set_stmt_row <- function(model, set_name) {
  rows <- which(model$type %in% "Set" & toupper(model$name) %in% toupper(set_name))
  limit_row <- if (length(rows) > 0L) {
    rows[1]
  } else {
    nrow(model) + 1L
  }
  return(limit_row)
}
