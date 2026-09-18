#' @keywords internal
#' @noRd
.coerce_terms <- function(node) {
  if (node$kind %=% "linear") {
    return(node$terms)
  }
  terms <- list(.new_term(fac = node$text, ops = "*"))
  return(terms)
}
