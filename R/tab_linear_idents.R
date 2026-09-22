#' @keywords internal
#' @noRd
.expr_idents <- function(text) {
  tokens <- regmatches(
    text,
    gregexpr("\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_]*", text, perl = TRUE)
  )[[1]]
  return(tokens[!grepl("^\"", tokens)])
}
