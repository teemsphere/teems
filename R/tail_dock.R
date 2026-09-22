#' @keywords internal
#' @noRd
.dock_tail <- function(string) {
  docked_str <- sub("[a-z][a-z0-9]*$", "", string)

  return(docked_str)
}
