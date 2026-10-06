#' @keywords internal
#' @noRd
.expr_idents_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.expr_idents <- function(text) {
  if (!nzchar(text)) {
    tokens <- character(0)
    return(tokens)
  }
  hit <- .expr_idents_cache[[text]]
  if (!is.null(hit)) {
    return(hit)
  }
  tokens <- regmatches(
    text,
    gregexpr("\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_@]*", text, perl = TRUE)
  )[[1]]
  tokens <- tokens[!grepl("^\"", tokens)]
  assign(text, tokens, envir = .expr_idents_cache)
  return(tokens)
}
