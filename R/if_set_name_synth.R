#' @keywords internal
#' @noRd
.synth_set_name <- function(synth) {
  repeat {
    synth$n <- synth$n + 1L
    nm <- paste0("IFS", synth$n)
    if (!toupper(nm) %in% synth$names) {
      return(nm)
    }
  }
}
