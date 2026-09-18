#' @keywords internal
#' @noRd
.solve_lock_release <- function(lock_path) {
  if (!is.null(lock_path)) {
    unlink(lock_path, recursive = TRUE, force = TRUE)
  }
  return(invisible(NULL))
}
