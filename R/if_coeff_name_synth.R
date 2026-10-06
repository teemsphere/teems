#' @keywords internal
#' @noRd
.synth_coeff_name <- function(synth) {
  if (is.null(synth$nc)) {
    synth$nc <- 0L
  }
  repeat {
    synth$nc <- synth$nc + 1L
    nm <- paste0("IFC", synth$nc)
    if (!toupper(nm) %in% synth$names) {
      return(nm)
    }
  }
}
