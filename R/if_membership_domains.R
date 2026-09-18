# pass 2: the domains the membership conditions cut the equation into
# -- one intersection set per condition and the complement that carries
# the remainder
#' @keywords internal
#' @noRd
.if_membership_domains <- function(membership,
                                   quant,
                                   q_idx,
                                   synth,
                                   stmt,
                                   pre,
                                   call) {
  at <- NA_integer_
  inter_names <- character(0)
  for (m in membership) {
    cond_info <- m$cond_info
    at_m <- match(tolower(cond_info$idx), tolower(q_idx))
    if (is.na(at_m) || isTRUE(quant[[at_m]]$cond) ||
      (cond_info$kind %=% "in_set" &&
        toupper(cond_info$set) %in% toupper(q_idx[!is.na(q_idx)]))) {
      if_cond <- m$if_cond
      .cli_action(model_err$invalid_if_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (!is.na(at) && at_m != at) {
      # membership terms on different indices would need a nested
      # (product) split; no measured model demand
      if_statement <- stmt
      .cli_action(model_err$invalid_if_multi,
        action = c("abort", "inform"),
        call = call
      )
    }
    at <- at_m
    range_set <- quant[[at]]$set
    operand <- if (cond_info$kind %=% "in_set") {
      cond_info$set
    } else {
      paste0('"', cond_info$elem, '"')
    }
    inter <- .synth_intersect_set(operand, range_set, synth)
    pre <- c(pre, inter$pre)
    inter_names <- c(inter_names, inter$name)
  }
  idx_name <- membership[[1]]$cond_info$idx
  range_set <- quant[[at]]$set

  # remainder domain: range - S1 (one term) or range - (S1 + S2 + ...);
  # the '+' is disjointness-checked at set resolution
  union_expr <- if (length(inter_names) %=% 1L) {
    inter_names
  } else {
    paste0("(", paste(inter_names, collapse = " + "), ")")
  }
  comp_key <- toupper(paste0(range_set, "-", union_expr))
  comp <- synth[[comp_key]]
  if (is.null(comp)) {
    comp <- .synth_set_name(synth)
    synth[[comp_key]] <- comp
    pre <- c(pre, sprintf(
      "Set %s # if-rewrite %s minus %s # = %s - %s",
      comp, range_set, union_expr, range_set, union_expr
    ))
  }
  domains <- list(pre = pre, at = at, inter_names = inter_names,
                  idx_name = idx_name, range_set = range_set, comp = comp)
  return(domains)
}
