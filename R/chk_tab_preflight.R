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

#' @keywords internal
#' @noRd
.formula_lhs_name <- function(comp1) {
  x <- tolower(comp1)
  x <- gsub("^\\s*(\\([^)]*\\)\\s*)*", "", x)
  lhs_name <- sub("^([a-z][a-z0-9_]*).*$", "\\1", x)
  return(lhs_name)
}
