#' Names of the declared LINEAR variables (every Variable statement
#' whose qualifiers do not say levels), upper-cased.
#'
#' @keywords internal
#' @noRd
.tab_linear_variable_names <- function(tab) {
  stmts <- tab[grepl("^\\s*variable\\b", tab, ignore.case = TRUE)]
  if (length(stmts) == 0L) {
    var_names <- character(0)
    return(var_names)
  }
  quals <- vapply(stmts, \(st) {
    body <- sub("^\\s*variable\\s*", "", st, ignore.case = TRUE)
    body <- gsub("#[^#]*#", "", body)
    paste(regmatches(body, gregexpr("^\\s*(\\([^)]*\\)\\s*)*", body))[[1]], collapse = "")
  }, character(1), USE.NAMES = FALSE)
  quals <- gsub("\\(all\\s*,[^)]*\\)", "", quals, ignore.case = TRUE)
  linear <- !grepl("levels", quals, ignore.case = TRUE)
  var_names <- .tab_declared_names(stmts[linear], "variable")
  return(var_names)
}
