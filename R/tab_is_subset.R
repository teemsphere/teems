#' @keywords internal
#' @noRd
.tab_is_subset <- function(sub, sup, synth) {
  if (toupper(sub) %=% toupper(sup)) {
    return(TRUE)
  }
  if (is.null(synth$subsets)) {
    synth$subsets <- .tab_subset_closure(synth$tab)
  }
  return(toupper(sup) %in% synth$subsets[[toupper(sub)]])
}
