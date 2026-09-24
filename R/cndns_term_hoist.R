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
  bound <- tolower(names(binding))
  fac_dims <- lapply(term$fac, \(f) intersect(tolower(.expr_idents(f)), bound))
  if (length(unique(unlist(fac_dims))) > max(lengths(fac_dims))) {
    return(term)
  }
  expr <- term$fac[[1]]
  for (f in seq_along(term$fac)[-1]) {
    expr <- paste0(expr, term$ops[[f]], term$fac[[f]])
  }
  ref <- .csub_new(
    expr = expr,
    binding = .binding_used(binding, expr),
    label = label,
    csub = csub
  )
  term$fac <- ref
  term$ops <- "*"
  return(term)
}
