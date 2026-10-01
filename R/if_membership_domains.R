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
      .if_native()
    }
    if (!is.na(at) && at_m != at) {
      .if_native()
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
