#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.synth_eq_names <- function(name, synth, n = 2L) {
  eq_names <- purrr::map_chr(LETTERS[seq_len(n)], \(suffix) {
    nm <- paste0(name, suffix)
    while (toupper(nm) %in% synth$names) {
      nm <- paste0(nm, suffix)
    }
    nm
  })
  return(eq_names)
}
