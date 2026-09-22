#' @keywords internal
#' @noRd
.tab_qualifier_groups <- function(text) {
  text <- gsub("#[^#]*#", "", text)
  rest <- sub("^\\s*[A-Za-z_]+", "", text)
  groups <- character(0)
  unbalanced <- FALSE
  repeat {
    if (!grepl("^\\s*\\(", rest)) {
      break
    }
    if (grepl("^\\s*\\(\\s*all[ ,]", rest, ignore.case = TRUE)) {
      break
    }
    m <- regmatches(rest, regexec("^\\s*\\(([^)]*)\\)", rest))[[1]]
    if (length(m) == 0L) {
      unbalanced <- TRUE
      break
    }
    groups <- c(groups, m[2])
    rest <- sub("^\\s*\\([^)]*\\)", "", rest)
  }
  groups <- list(groups = groups, unbalanced = unbalanced)
  return(groups)
}
