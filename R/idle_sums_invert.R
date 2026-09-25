#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.invert_idle_sums <- function(term,
                              binding,
                              label,
                              csub) {
  if (length(term$quants) == 0L || length(term$fac) == 0L) {
    term <- .hoist_term(
      term = term,
      binding = c(binding, .quant_binding(term$quants)),
      label = label,
      csub = csub,
      min_fac = 3L
    )
    return(term)
  }

  var_idents <- character()
  if (!is.null(term$var)) {
    var_idents <- unlist(lapply(term$var$args, .expr_idents))
  }

  idle <- purrr::map_lgl(term$quants, \(q) {
    !q$idx %in% var_idents
  })

  if (!any(idle)) {
    term <- .hoist_term(
      term = term,
      binding = c(binding, .quant_binding(term$quants)),
      label = label,
      csub = csub,
      min_fac = 3L
    )
    return(term)
  }

  expr <- .fac_text(term$fac, term$ops)
  for (q in rev(term$quants[idle])) {
    expr <- paste0("sum{", q$idx, ",", q$set, ", ", expr, "}")
  }

  keep <- term$quants[!idle]
  full_binding <- c(binding, .quant_binding(keep))
  ref <- .csub_new(
    expr = expr,
    binding = .binding_used(full_binding, expr),
    label = label,
    csub = csub
  )

  term$fac <- ref
  term$ops <- "*"
  term$quants <- keep
  return(term)
}
