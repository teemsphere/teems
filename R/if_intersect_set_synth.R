#' @keywords internal
#' @noRd
.synth_intersect_set <- function(operand, range_set, synth) {
  if (grepl("^[A-Za-z_][A-Za-z0-9_@]*$", operand) &&
    .tab_is_subset(operand, range_set, synth)) {
    intersect <- list(pre = character(0), name = operand)
    return(intersect)
  }
  key <- toupper(paste0(operand, "&", range_set))
  nm <- synth[[key]]
  pre <- character(0)
  if (is.null(nm)) {
    nm <- .synth_set_name(synth)
    synth[[key]] <- nm
    pre <- sprintf(
      "Set %s # if-rewrite %s intersect %s # = %s & %s",
      nm, operand, range_set, operand, range_set
    )
  }
  intersect <- list(pre = pre, name = nm)
  return(intersect)
}
