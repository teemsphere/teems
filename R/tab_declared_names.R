#' @keywords internal
#' @noRd
.tab_declared_names <- function(tab, type) {
  stmts <- tab[grepl(paste0("^\\s*", type, "\\b"), tab, ignore.case = TRUE)]
  if (length(stmts) == 0L) {
    declared <- character(0)
    return(declared)
  }
  x <- sub(paste0("^\\s*", type, "\\s*"), "", stmts, ignore.case = TRUE)
  x <- gsub("#[^#]*#", "", x)
  x <- gsub("\\(all\\s*,[^)]*\\)", "", x, ignore.case = TRUE)
  x <- gsub("^\\s*(\\([^)]*\\)\\s*)*", "", x)
  declared <- toupper(sub("^\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", x))
  return(declared)
}
