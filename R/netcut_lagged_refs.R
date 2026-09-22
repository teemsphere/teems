#' @importFrom tibble as_tibble
#' @keywords internal
#' @noRd
.netcut_lagged_refs <- function(stmt, vars) {
  hits <- gregexpr("([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^()]*)\\)", stmt)[[1]]
  out <- list(name = character(0), start = integer(0), end = integer(0), args = list())
  if (hits[1] %=% -1L) {
    refs <- tibble::as_tibble(out)
    return(refs)
  }
  for (i in seq_along(hits)) {
    start <- hits[i]
    end <- start + attr(hits, "match.length")[i] - 1L
    ref <- substr(stmt, start, end)
    nm <- tolower(sub("\\s*\\(.*$", "", ref))
    if (!nm %in% vars$name) {
      next
    }
    args <- trimws(strsplit(sub("^[^(]*\\(", "", sub("\\)$", "", ref)), ",")[[1]])
    has_offset <- any(grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*[+-]\\s*[0-9]+$", args))
    has_elem <- any(grepl("^\"[^\"]*\"$", args))
    if (!has_offset || !has_elem) {
      next
    }
    if (length(args) %!=% length(vars$idx[[match(nm, vars$name)]])) {
      next
    }
    out$name <- c(out$name, nm)
    out$start <- c(out$start, start)
    out$end <- c(out$end, end)
    out$args <- c(out$args, list(args))
  }
  refs <- tibble::as_tibble(out)
  return(refs)
}
