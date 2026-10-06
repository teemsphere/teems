#' @keywords internal
#' @noRd
.synth_expr_name <- function(synth) {
  if (is.null(synth$nx)) {
    synth$nx <- 0L
  }
  repeat {
    synth$nx <- synth$nx + 1L
    nm <- paste0("IFX", synth$nx)
    if (!toupper(nm) %in% synth$names) {
      return(nm)
    }
  }
}
