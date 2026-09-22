#' @keywords internal
#' @noRd
.synth_proxy_name <- function(synth) {
  repeat {
    synth$n <- synth$n + 1L
    nm <- paste0("NCV", synth$n)
    hit <- paste0("(^|[^A-Za-z0-9_])(E_)?", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}
