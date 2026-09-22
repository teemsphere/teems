#' @keywords internal
#' @noRd
.rename_expr_tokens <- function(text, map) {
  if (length(map) == 0L || !nzchar(text)) {
    return(text)
  }
  pattern <- "\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_]*"
  m <- gregexpr(pattern, text, perl = TRUE)
  tokens <- regmatches(text, m)[[1]]
  hit <- !grepl("^\"", tokens) & tokens %in% names(map)
  tokens[hit] <- map[tokens[hit]]
  regmatches(text, m)[[1]] <- tokens
  return(text)
}
