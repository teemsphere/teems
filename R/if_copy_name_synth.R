#' @keywords internal
#' @noRd
.synth_copy_name <- function(synth) {
  if (is.null(synth$nt)) {
    synth$nt <- 0L
  }
  repeat {
    synth$nt <- synth$nt + 1L
    nm <- paste0("IFT", synth$nt)
    hit <- paste0("(^|[^A-Za-z0-9_@])", nm, "([^A-Za-z0-9_@]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}
