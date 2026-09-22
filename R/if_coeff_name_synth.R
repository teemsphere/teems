#' @keywords internal
#' @noRd
.synth_coeff_name <- function(synth) {
  if (is.null(synth$nc)) {
    synth$nc <- 0L
  }
  repeat {
    synth$nc <- synth$nc + 1L
    nm <- paste0("IFC", synth$nc)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}
