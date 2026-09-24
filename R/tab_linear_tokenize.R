#' @keywords internal
#' @noRd
.tokenize_expr <- function(text) {
  pattern <- paste0(
    "\"[^\"]*\"",
    "|[A-Za-z_][A-Za-z0-9_@]*",
    "|(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?",
    "|[-+*/^(),\\[\\]{}:=<>]"
  )
  tokens <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  leftover <- gsub(pattern, "", text, perl = TRUE)
  leftover <- gsub("\\s", "", leftover)
  if (nchar(leftover) > 0L) {
    stop(sprintf(model_err$linear_reason$unrecognized, leftover), call. = FALSE)
  }
  return(tokens)
}
