#' @noRd
#' @keywords internal
.usr_shock <- function(shock_file) {
  class(shock_file) <- "user"
  shock <- list(shock_file)
  class(shock) <- c("shock", class(shock))
  attr(shock, "file") <- basename(shock_file)
  return(shock)
}