#' @keywords internal
#' @noRd
.set_expr_tokens <- function(d) {
  d <- sub("^\\s*=\\s*", "", d)
  d <- gsub("\\\\", "-", d)
  d <- gsub("union", " ^ ", d, ignore.case = TRUE)
  d <- gsub("intersect", " & ", d, ignore.case = TRUE)
  d <- gsub("(?<=[[:space:])])[xX](?=[[:space:](])", " * ", d, perl = TRUE)
  m <- gregexpr('"[^"]*"|[()+^&*-]|[^()+^&*"[:space:]-]+', d)[[1]]
  if (m[1] %=% -1L) {
    tokens <- character(0)
    return(tokens)
  }
  return(regmatches(d, list(m))[[1]])
}
