#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.rename_term <- function(term, map) {
  term$fac <- purrr::map_chr(term$fac, .rename_expr_tokens, map = map)
  term$quants <- lapply(term$quants, \(q) {
    at <- match(tolower(q$idx), tolower(names(map)))
    if (!is.na(at)) {
      q$idx <- unname(map[[at]])
    }
    if (!is.null(q$cond)) {
      q$cond <- .rename_expr_tokens(q$cond, map = map)
    }
    q
  })
  if (!is.null(term$var) && length(term$var$args) > 0L) {
    term$var$args <- purrr::map_chr(term$var$args, .rename_expr_tokens, map = map)
  }
  return(term)
}
