#' @importFrom purrr map_chr map_lgl
#' @keywords internal
#' @noRd
.if_self_copy <- function(lhs, sym, quant, qual_groups, synth) {
  decl <- .tab_coeff_decl(synth$tab, sym)
  if (is.null(decl)) {
    qs <- quant[purrr::map_lgl(quant, "is_quant")]
    decl <- list(
      quants = paste0(purrr::map_chr(qs, \(q) sprintf("(all,%s,%s)", q$idx, q$set)), collapse = ""),
      args = gsub("\\s", "", sub("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*", "", lhs))
    )
  }
  nm <- .synth_copy_name(synth)
  qual <- if (length(qual_groups) > 0L) {
    paste0(paste(qual_groups, collapse = ""), " ")
  } else {
    ""
  }
  sep <- if (nzchar(decl$quants)) {
    " "
  } else {
    ""
  }
  copy <- list(
    pre = c(
      sprintf("Coefficient %s%s%s # if-rewrite copy of %s #", decl$quants, sep, paste0(nm, decl$args), sym),
      sprintf("Formula %s%s%s%s = %s", qual, decl$quants, sep, paste0(nm, decl$args), paste0(sym, decl$args))
    ),
    name = nm
  )
  return(copy)
}
