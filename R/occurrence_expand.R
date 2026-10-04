#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.expand_occurrence <- function(t,
                               def_args,
                               solution,
                               entry,
                               used,
                               csub) {
  base_map <- stats::setNames(t$var$args, def_args)
  base_map <- base_map[tolower(names(base_map)) != tolower(base_map)]
  out <- list()

  for (s in solution) {
    fresh_map <- character()
    for (q in s$quants) {
      if (tolower(q$idx) %in% tolower(c(used, t$var$args))) {
        candidate_pool <- paste0(q$idx, seq_len(99L))
        fresh <- candidate_pool[!tolower(candidate_pool) %in% tolower(used)][[1]]
        fresh_map[[q$idx]] <- fresh
        used <- c(used, fresh)
      }
    }

    s2 <- .rename_term(s, c(base_map, fresh_map))

    new <- .new_term(
      sign = t$sign * s$sign,
      quants = c(t$quants, s2$quants),
      fac = c(t$fac, s2$fac),
      ops = c(t$ops, s2$ops),
      var = s2$var
    )

    new <- .invert_idle_sums(
      term = new,
      binding = .quant_binding(entry$quants),
      label = paste0("backsolve product (", entry$name, ")"),
      csub = csub
    )

    out <- c(out, list(new))
  }

  expanded <- list(terms = out, used = used)
  return(expanded)
}
