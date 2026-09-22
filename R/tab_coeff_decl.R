#' @keywords internal
#' @noRd
.tab_coeff_decl <- function(tab, sym) {
  stmts <- tab[grepl("^\\s*[Cc][Oo][Ee][Ff][Ff][Ii][Cc][Ii][Ee][Nn][Tt]\\b", tab)]
  for (st in stmts) {
    body <- sub("^\\s*[Cc][Oo][Ee][Ff][Ff][Ii][Cc][Ii][Ee][Nn][Tt]\\s*", "", st)
    body <- gsub("#[^#]*#", " ", body)
    groups <- character(0)
    repeat {
      body <- sub("^\\s+", "", body)
      if (!startsWith(body, "(")) {
        break
      }
      close <- .match_bracket(body, 1L)
      if (is.na(close)) {
        break
      }
      groups <- c(groups, substr(body, 1L, close))
      body <- substring(body, close + 1L)
    }
    nm <- sub("^\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", body)
    if (!toupper(nm) %=% toupper(sym)) {
      next
    }
    rest <- sub("^\\s*[A-Za-z_][A-Za-z0-9_]*\\s*", "", body)
    args <- ""
    if (startsWith(rest, "(")) {
      close <- .match_bracket(rest, 1L)
      if (!is.na(close)) {
        args <- gsub("\\s", "", substr(rest, 1L, close))
      }
    }
    quants <- groups[grepl("^\\(\\s*[Aa][Ll][Ll]\\s*,", groups)]
    decl <- list(quants = paste0(gsub("\\s", "", quants), collapse = ""), args = args)
    return(decl)
  }
  return(NULL)
}
