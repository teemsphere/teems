#' @keywords internal
#' @noRd
.pk <- function(st) {
  if (st$pos > length(st$tokens)) {
    return(NA_character_)
  }
  return(st$tokens[[st$pos]])
}
