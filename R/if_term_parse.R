#' Is this term exactly one IF call? Returns its condition and value
#' (split at the first top-level comma) or NULL.
#'
#' @keywords internal
#' @noRd
.parse_if_term <- function(body) {
  if (!grepl("^[Ii][Ff]\\s*[][({]", body)) {
    return(NULL)
  }
  open <- regexpr("[][({]", body)
  close <- .match_bracket(body, open)
  if (is.na(close) || nchar(trimws(substring(body, close + 1L))) > 0L) {
    return(NULL)
  }
  inner <- substr(body, open + 1L, close - 1L)
  scan <- .tab_scan(inner)
  comma <- which(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote)
  if (length(comma) %=% 0L) {
    return(NULL)
  }
  term <- list(
    cond = trimws(substr(inner, 1L, comma[1] - 1L)),
    value = trimws(substring(inner, comma[1] + 1L))
  )
  return(term)
}
