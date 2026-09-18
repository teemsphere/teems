#' @keywords internal
#' @noRd
.new_term <- function(sign = 1L,
                      quants = list(),
                      fac = character(),
                      ops = character(),
                      var = NULL) {
  term <- list(sign = sign, quants = quants, fac = fac, ops = ops, var = var)
  return(term)
}
