#' @keywords internal
#' @noRd
.negate_terms <- function(terms) {
  negated <- lapply(terms, \(t) {
    t$sign <- -t$sign
    t
  })
  return(negated)
}
