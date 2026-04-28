#' @keywords internal
#' @noRd
.GTAPv7_set <- function(header = NULL,
                        name = NULL,
                        class = NULL,
                        ele) {
  if (is.null(header)) {
    header <- class[[1]]
  }

  if (is.null(class)) {
    class <- c(header, name, "set", "GTAPv7", "character")
  }

  class(ele) <- class
  return(ele)
}