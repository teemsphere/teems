#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.synth_eq_names <- function(name, synth, n = 2L) {
  eq_names <- purrr::map_chr(LETTERS[seq_len(n)], \(suffix) {
    nm <- paste0(name, suffix)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    while (any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      nm <- paste0(nm, suffix)
      hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    }
    nm
  })
  return(eq_names)
}
