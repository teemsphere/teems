#' @keywords internal
#' @noRd
.if_indicator_scalar <- function(cond_info,
                                 synth,
                                 if_cond,
                                 call) {
  key <- paste0(
    "CMP|", toupper(cond_info$ref), "|", cond_info$op, "|", cond_info$num
  )
  ind <- synth[[key]]
  pre <- character(0)
  if (is.null(ind)) {
    ind <- .synth_coeff_name(synth)
    synth[[key]] <- ind
    ref_name <- sub("\\(.*$", "", cond_info$ref)
    qual <- if (.tab_coef_is_param(synth$tab, ref_name)) "(parameter) " else ""
    pre <- c(
      sprintf("Coefficient %s%s # if-rewrite indicator %s #", qual, ind, if_cond),
      .rewrite_formula_if(
        sprintf("Formula %s = if(%s %s %s, 1)", ind, cond_info$ref, cond_info$op, cond_info$num),
        synth,
        call
      )
    )
  }
  indicator <- list(pre = pre, ref = ind)
  return(indicator)
}
