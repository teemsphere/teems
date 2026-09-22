#' @keywords internal
#' @noRd
.stmt_index_sets <- function(text) {
  pairs <- regmatches(text, gregexpr(
    "(\\(\\s*all|\\bsum\\s*[{(])\\s*,?\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)",
    text, ignore.case = TRUE, perl = TRUE
  ))[[1]]
  if (length(pairs) == 0L) {
    sets <- character()
    return(sets)
  }
  parts <- regmatches(pairs, gregexpr("[A-Za-z_][A-Za-z0-9_]*", pairs))
  idx <- tolower(vapply(parts, \(p) p[[length(p) - 1L]], character(1)))
  set <- vapply(parts, \(p) p[[length(p)]], character(1))
  out <- character()
  for (k in seq_along(idx)) {
    if (idx[[k]] %in% names(out)) {
      prev <- out[[idx[[k]]]]
      if (!is.na(prev) && tolower(prev) != tolower(set[[k]])) {
        out[[idx[[k]]]] <- NA_character_
      }
      next
    }
    out[[idx[[k]]]] <- set[[k]]
  }
  return(out[!is.na(out)])
}
