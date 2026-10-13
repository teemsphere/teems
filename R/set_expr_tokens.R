#' @keywords internal
#' @noRd
.set_expr_tokens <- function(d) {
  d <- sub("^\\s*=\\s*", "", d)
  d <- gsub("\\\\", " % ", d)
  outside <- "(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"
  d <- gsub(paste0("\\bunion\\b", outside), " ^ ", d, ignore.case = TRUE, perl = TRUE)
  d <- gsub(paste0("\\bintersect\\b", outside), " & ", d, ignore.case = TRUE, perl = TRUE)
  d <- gsub("(?<=[[:space:])])[xX](?=[[:space:](])", " * ", d, perl = TRUE)
  m <- gregexpr('"[^"]*"|[()+^&*%-]|[^()+^&*%"[:space:]-]+', d)[[1]]
  if (m[1] %=% -1L) {
    tokens <- character(0)
    return(tokens)
  }
  return(regmatches(d, list(m))[[1]])
}
