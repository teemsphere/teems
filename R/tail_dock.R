#' @keywords internal
#' @noRd
.dock_tail <- function(string) {
  # docked_str <- substr(
  #   string,
  #   1,
  #   nchar(string) - length
  # )
  docked_str <- sub("[a-z]+$", "", string)

  return(docked_str)
}