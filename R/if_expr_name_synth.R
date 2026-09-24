#' @keywords internal
#' @noRd
.synth_expr_name <- function(synth) {
  if (is.null(synth$nx)) {
    synth$nx <- 0L
  }
  repeat {
    synth$nx <- synth$nx + 1L
    nm <- paste0("IFX", synth$nx)
    hit <- paste0("(^|[^A-Za-z0-9_@])", nm, "([^A-Za-z0-9_@]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}
