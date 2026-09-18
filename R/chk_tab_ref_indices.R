#' @keywords internal
#' @noRd
# NAME(<index>, ...) references in the math statements: an empty index
# (x( ), x(c,,t)) reached the solver's operand binder as a NULL token
# (fuzz batch 13: SEGV; now a named solver abort). Depth-aware split of
# each argument list at its top-level commas; sum(...) has its own check
# above and (all,...) quantifiers never carry an empty piece.
.chk_tab_ref_indices <- function(statements,
                                 call) {
  no_label <- gsub("#[^#]*#", "", statements)
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", no_label))
  math <- kw %in% c("equation", "formula", "update", "assertion")
  for (i in which(math)) {
    # quoted element literals are opaque
    txt <- gsub("\"[^\"]*\"", "\"\"", no_label[i])
    starts <- gregexpr("[A-Za-z_][A-Za-z0-9_]*\\s*\\(", txt)[[1]]
    if (starts[1] == -1L) {
      next
    }
    lens <- attr(starts, "match.length")
    chars <- strsplit(txt, "", fixed = TRUE)[[1]]
    for (k in seq_along(starts)) {
      name <- tolower(trimws(sub("\\($", "", substr(txt, starts[k], starts[k] + lens[k] - 1L))))
      if (name == "sum") {
        next
      }
      j <- starts[k] + lens[k] - 1L
      depth <- 0L
      pieces <- character()
      cur <- ""
      closed <- FALSE
      while (j <= length(chars)) {
        ch <- chars[j]
        if (ch == "(") {
          depth <- depth + 1L
          if (depth > 1L) {
            cur <- paste0(cur, ch)
          }
        } else if (ch == ")") {
          depth <- depth - 1L
          if (depth == 0L) {
            pieces <- c(pieces, cur)
            closed <- TRUE
            break
          }
          cur <- paste0(cur, ch)
        } else if (ch == "," && depth == 1L) {
          pieces <- c(pieces, cur)
          cur <- ""
        } else {
          cur <- paste0(cur, ch)
        }
        j <- j + 1L
      }
      if (!closed) {
        next
      }
      if (any(!nzchar(trimws(pieces)))) {
        bad_ref <- trimws(substr(txt, starts[k], j))
        bad_stmt <- trimws(statements[i])
        .cli_action(model_err$ref_index_empty,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}
