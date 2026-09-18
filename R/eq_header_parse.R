# the statement's own fields: a leading qualifier, the equation name,
# its # label # and the quantifier groups that follow
#' @keywords internal
#' @noRd
.parse_eq_header <- function(stmt) {
  rest <- trimws(sub("^\\s*[Ee][Qq][Uu][Aa][Tt][Ii][Oo][Nn]\\s*", "", stmt))

  qual <- ""
  if (startsWith(rest, "(")) {
    close <- .match_bracket(rest, 1L)
    qual <- paste0(substr(rest, 1L, close), " ")
    rest <- trimws(substring(rest, close + 1L))
  }
  name <- sub("^([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", rest)
  rest <- trimws(sub("^[A-Za-z_][A-Za-z0-9_]*", "", rest))

  label <- ""
  if (startsWith(rest, "#")) {
    m <- regexpr("^#[^#]*#", rest)
    label <- paste0(substr(rest, 1L, attr(m, "match.length")), " ")
    rest <- trimws(substring(rest, attr(m, "match.length") + 1L))
  }

  groups <- character(0)
  repeat {
    rest <- sub("^\\s+", "", rest)
    if (!startsWith(rest, "(")) {
      break
    }
    close <- .match_bracket(rest, 1L)
    groups <- c(groups, substr(rest, 1L, close))
    rest <- substring(rest, close + 1L)
  }
  header <- list(rest = rest, qual = qual, name = name, label = label, groups = groups)
  return(header)
}
