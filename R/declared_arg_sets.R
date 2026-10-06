#' @keywords internal
#' @noRd
.declared_arg_sets <- function(extract) {
  decl_rows <- which(tolower(extract$type) %in% c("coefficient", "variable"))
  out <- list()
  if (length(decl_rows) == 0L) {
    return(out)
  }
  texts <- .strip_tab_labels(extract$remainder[decl_rows])
  idx_list <- .stmts_index_sets(texts)
  body <- gsub("\\(\\s*all\\s*,[^)]*\\)", " ", texts, ignore.case = TRUE)
  body <- gsub("\\([^()]*=[^()]*\\)|\\(\\s*(parameter|integer|real|levels|linear|change|percent_change|non_parameter|initial|always|ge|gt|le|lt)\\b[^()]*\\)", " ", body, ignore.case = TRUE)
  m <- regmatches(body, regexec("([A-Za-z_][A-Za-z0-9_@]*)\\s*\\(([^()]*)\\)", body))
  for (i in seq_along(decl_rows)) {
    if (length(m[[i]]) == 0L) {
      next
    }
    args <- trimws(strsplit(m[[i]][[3]], ",")[[1]])
    if (length(args) == 0L || any(!nzchar(args))) {
      next
    }
    idx_sets <- idx_list[[i]]
    sets <- unname(idx_sets[tolower(args)])
    if (any(is.na(sets))) {
      next
    }
    out[[tolower(m[[i]][[2]])]] <- sets
  }
  return(out)
}
