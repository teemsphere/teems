#' @keywords internal
#' @noRd
.hoist_term <- function(term,
                        binding,
                        label,
                        csub,
                        min_fac = 2L) {
  if (length(term$fac) < min_fac) {
    return(term)
  }
  if (all(grepl("^-?(?:[0-9.]|\")", term$fac))) {
    return(term)
  }
  expr <- term$fac[[1]]
  for (f in seq_along(term$fac)[-1]) {
    expr <- paste0(expr, term$ops[[f]], term$fac[[f]])
  }
  used <- intersect(names(binding), .expr_idents(expr))
  ref <- .csub_new(
    expr = expr,
    binding = binding[used],
    label = label,
    csub = csub
  )
  term$fac <- ref
  term$ops <- "*"
  return(term)
}
