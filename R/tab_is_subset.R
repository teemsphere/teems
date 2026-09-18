#' Is `sub` the set `sup` or a declared subset of it? The relations
#' come from the raw statements (cached on `synth`): `Subset A is
#' subset of B`, and the definitions TABLO itself treats as declaring
#' them (manual 10.1): `Set X = A + B` (A, B in X), `Set X = A - B`
#' (X in A), `Set X = A & B` (X in A and B), `Set X = A` (both ways),
#' `Set X = (all,i,S: ...)` (X in S); transitively closed. Anything
#' else infers nothing, so a miss only costs a synthesized set.
#'
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
