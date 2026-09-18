#' A set header carrying `ele`. A user set never aggregates (the
#' disaggregated lists and mappings the model reads at source
#' resolution); the rest follow the mapping of the set they subset.
#'
#' @keywords internal
#' @noRd
.layer_set <- function(header, ele, fmt, user_set = TRUE) {
  s <- ele
  class(s) <- c(header, header, "set", fmt, "character")
  if (user_set) {
    attr(s, "user_set") <- TRUE
  }
  return(s)
}
