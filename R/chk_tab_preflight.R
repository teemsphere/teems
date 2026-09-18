#' Pre-flight TAB validation against the solver's fatal invariants
#'
#' Statement-table checks mirroring the solver-side fatals inventoried
#' in dev/validation_table.md (names 11.2.1, qualifiers 10.3/10.4,
#' bounds 10.19.1, Default statements 10.19, Reads 10.6/11.11.8,
#' PostSim scope 12.2.1-12.2.3). Aborting here fails the model before
#' the Docker deploy round-trip; the solver remains authoritative for
#' everything expression- or data-dependent.
#'
#' @keywords internal
#' @noRd
.check_tab_preflight <- function(model,
                                 call) {
  .chk_tab_names(model, call = call)
  .chk_tab_reads(model, call = call)
  .chk_tab_setbuilders(model, call = call)
  .chk_tab_postsim(model, call = call)
  .chk_tab_comp(model, call = call)
  return(invisible(NULL))
}

#' PostSim Formula LHS identifier (leading quantifier groups stripped)
#'
#' @keywords internal
#' @noRd
.formula_lhs_name <- function(comp1) {
  x <- tolower(comp1)
  x <- gsub("^\\s*(\\([^)]*\\)\\s*)*", "", x)
  lhs_name <- sub("^([a-z][a-z0-9_]*).*$", "\\1", x)
  return(lhs_name)
}
