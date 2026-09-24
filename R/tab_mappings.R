#' @keywords internal
#' @noRd
.tab_mappings <- function(tab) {
  stmts <- tab[grepl("^\\s*[Mm][Aa][Pp][Pp][Ii][Nn][Gg]\\b", tab)]
  out <- list()
  for (st in stmts) {
    body <- sub("^\\s*[Mm][Aa][Pp][Pp][Ii][Nn][Gg]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    body <- sub("^\\s*(\\([^)]*\\)\\s*)*", "", body)
    m <- regmatches(body, regexec(
      "^\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s+[Ff][Rr][Oo][Mm]\\s+([A-Za-z_][A-Za-z0-9_@]*)\\s+[Tt][Oo]\\s+([A-Za-z_][A-Za-z0-9_@]*)",
      body
    ))[[1]]
    if (length(m) > 0L) {
      out[[toupper(m[2])]] <- toupper(c(m[3], m[4]))
    }
  }
  return(out)
}
