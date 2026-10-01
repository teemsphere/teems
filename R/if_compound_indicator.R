#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.if_compound_indicator <- function(cond_info,
                                   quant,
                                   q_idx,
                                   synth,
                                   if_cond,
                                   call) {
  live <- which(!is.na(q_idx))
  used <- vapply(q_idx[live], \(ix) {
    grepl(paste0("(^|[^A-Za-z0-9_@\"])", ix, "([^A-Za-z0-9_@\"]|$)"), cond_info$cond, ignore.case = TRUE)
  }, logical(1))
  at <- live[used]
  if (length(at) %=% 0L) {
    .if_native()
  }
  dims <- q_idx[at]
  sets <- purrr::map_chr(quant[at], "set")
  key <- paste0("CMPD|", toupper(gsub("\\s", "", cond_info$cond)), "|", paste(toupper(dims), toupper(sets), collapse = ","))
  ind <- synth[[key]]
  pre <- character(0)
  dimargs <- paste(dims, collapse = ",")
  if (is.null(ind)) {
    ind <- .synth_coeff_name(synth)
    synth[[key]] <- ind
    quants <- sprintf("(all,%s,%s)", dims, sets)
    n <- length(quants)
    quants_cond <- c(quants[-n], sprintf("(all,%s,%s: %s)", dims[n], sets[n], cond_info$cond))
    pre <- c(
      sprintf("Coefficient %s %s(%s) # if-rewrite indicator %s #", paste0(quants, collapse = ""), ind, dimargs, if_cond),
      sprintf("Formula %s %s(%s) = 0", paste0(quants, collapse = ""), ind, dimargs),
      sprintf("Formula %s %s(%s) = 1", paste0(quants_cond, collapse = ""), ind, dimargs)
    )
  }
  indicator <- list(pre = pre, ref = sprintf("%s(%s)", ind, dimargs))
  return(indicator)
}
