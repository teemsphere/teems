#' @keywords internal
#' @noRd
.flag_tab_cndns <- function(tab,
                            condensed) {
  tab$condense <- NA_character_
  tab$condense_eq <- NA_character_
  if (!is.null(condensed$flags) && nrow(condensed$flags) > 0L) {
    flag_key <- paste(condensed$flags$type, tolower(condensed$flags$name))
    tab_key <- paste(tab$type, tolower(tab$name))
    r_idx <- match(tab_key, flag_key)
    tab$condense <- condensed$flags$condense[r_idx]
    tab$condense_eq <- condensed$flags$condense_eq[r_idx]
  }
  return(tab)
}
