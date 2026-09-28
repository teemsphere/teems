#' @keywords internal
#' @noRd
.strip_strong_comments <- function(text,
                                   call) {
  m <- gregexpr("!\\[\\[!|!\\]\\]!", text, perl = TRUE)[[1]]
  if (m[1L] == -1L) {
    return(text)
  }
  starts <- as.integer(m)
  opens <- substring(text, starts + 1L, starts + 1L) == "["
  depth <- 0L
  keep_from <- 1L
  outer_open <- NA_integer_
  pieces <- character(0)
  for (k in seq_along(starts)) {
    if (opens[k]) {
      if (depth == 0L) {
        pieces <- c(pieces, substr(text, keep_from, starts[k] - 1L))
        outer_open <- starts[k]
      }
      depth <- depth + 1L
    } else if (depth > 0L) {
      depth <- depth - 1L
      if (depth == 0L) {
        keep_from <- starts[k] + 4L
      }
    }
  }
  if (depth > 0L) {
    open_line <- nchar(gsub("[^\n]", "", substr(text, 1L, outer_open))) + 1L
    .cli_action(model_err$unclosed_strong_comment,
      action = c("abort", "inform"),
      call = call
    )
  }
  pieces <- c(pieces, substr(text, keep_from, nchar(text)))
  paste(pieces, collapse = "")
}
