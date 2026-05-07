#' @keywords internal
#' @noRd
.check_named_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(NA)
  }
  if (is.null(names(dots)) || any(names(dots) == "")) {
    dots <- FALSE
  }
  return(dots)
}