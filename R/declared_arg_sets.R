#' @keywords internal
#' @noRd
.declared_arg_sets <- function(extract) {
  decl_rows <- which(tolower(extract$type) %in% c("coefficient", "variable"))
  out <- list()
  for (n in decl_rows) {
    text <- .strip_tab_labels(extract$remainder[[n]])
    idx_sets <- .stmt_index_sets(text)
    body <- gsub("\\(\\s*all\\s*,[^)]*\\)", " ", text, ignore.case = TRUE)
    body <- gsub("\\([^()]*=[^()]*\\)|\\(\\s*(parameter|integer|real|levels|linear|change|percent_change|non_parameter|initial|always|ge|gt|le|lt)\\b[^()]*\\)", " ", body, ignore.case = TRUE)
    m <- regmatches(body, regexec("([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^()]*)\\)", body))[[1]]
    if (length(m) == 0L) {
      next
    }
    args <- trimws(strsplit(m[[3]], ",")[[1]])
    if (length(args) == 0L || any(!nzchar(args))) {
      next
    }
    sets <- vapply(args, \(a) {
      a <- tolower(a)
      if (!is.na(idx_sets[a])) {
        idx_sets[[a]]
      } else {
        NA_character_
      }
    }, character(1))
    if (any(is.na(sets))) {
      next
    }
    out[[tolower(m[[2]])]] <- unname(sets)
  }
  return(out)
}
