#' @keywords internal
#' @noRd
.canonical_keywords <- function(statements) {
  kw <- sub("^\\s*([A-Za-z_]+).*$", "\\1", statements)
  canon <- supported_state[match(tolower(kw), tolower(supported_state))]
  hit <- !is.na(canon) & canon != kw
  statements[hit] <- paste0(canon[hit], sub("^\\s*[A-Za-z_]+", "", statements[hit]))
  return(statements)
}
