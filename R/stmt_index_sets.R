#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.stmts_index_sets <- function(texts) {
  pairs <- regmatches(texts, gregexpr(
    "(\\(\\s*all|\\bsum\\s*[{(])\\s*,?\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)",
    texts, ignore.case = TRUE, perl = TRUE
  ))
  n_pairs <- lengths(pairs)
  flat <- unlist(pairs, use.names = FALSE)
  if (length(flat) == 0L) {
    sets <- rep(list(character()), length(texts))
    return(sets)
  }
  parts <- regmatches(flat, gregexpr("[A-Za-z_][A-Za-z0-9_@]*", flat))
  idx <- tolower(vapply(parts, \(p) p[[length(p) - 1L]], character(1)))
  set <- vapply(parts, \(p) p[[length(p)]], character(1))
  stmt <- rep(seq_along(texts), n_pairs)
  key <- paste(stmt, idx)
  n_distinct <- lengths(lapply(split(tolower(set), key), unique))
  conflict <- key %in% names(n_distinct)[n_distinct > 1L]
  first <- !duplicated(key) & !conflict
  sets <- split(stats::setNames(set[first], idx[first]), factor(stmt[first], levels = seq_along(texts)))
  sets <- lapply(unname(sets), \(s) s)
  return(sets)
}

#' @keywords internal
#' @noRd
.stmt_index_sets <- function(text) {
  sets <- .stmts_index_sets(text)[[1]]
  return(sets)
}
