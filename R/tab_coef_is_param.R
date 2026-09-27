#' @keywords internal
#' @noRd
.tab_coef_is_param <- function(tab,
                               name) {
  decl <- tab[grepl("^\\s*[Cc][Oo][Ee][Ff][Ff][Ii][Cc][Ii][Ee][Nn][Tt]\\b", tab)]
  for (stmt in decl) {
    body <- sub("^\\s*[Cc][Oo][Ee][Ff][Ff][Ii][Cc][Ii][Ee][Nn][Tt]\\s*", "", stmt)
    groups <- character(0)
    repeat {
      body <- sub("^\\s+", "", body)
      if (!startsWith(body, "(")) {
        break
      }
      close <- .match_bracket(body, 1L)
      groups <- c(groups, tolower(substr(body, 2L, close - 1L)))
      body <- substring(body, close + 1L)
    }
    decl_name <- sub("^([A-Za-z_][A-Za-z0-9_@]*).*$", "\\1", body)
    if (!identical(tolower(decl_name), tolower(name))) {
      next
    }
    quals <- trimws(unlist(strsplit(groups[!grepl("^\\s*all\\s*,", groups)], ",")))
    is_param <- !"non_parameter" %in% quals && any(c("parameter", "integer") %in% quals)
    return(is_param)
  }
  return(FALSE)
}
