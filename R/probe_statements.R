#' @keywords internal
#' @noRd
.probe_statements <- function(s) {
  if (is.null(s) || !NROW(s)) {
    statements <- tibble::tibble(
      eq = character(),
      rows = integer(),
      dims = list(),
      vars = list()
    )
    return(statements)
  }
  statements <- tibble::tibble(
    eq = s$eq,
    rows = as.integer(s$rows),
    dims = s$dims,
    vars = lapply(s$vars, \(v) {
      if (is.null(v) || !NROW(v)) {
        tibble::tibble(var = character(), weight = integer())
      } else {
        tibble::tibble(var = v$v, weight = as.integer(v$w))
      }
    })
  )
  return(statements)
}
