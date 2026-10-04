#' @keywords internal
#' @noRd
.if_in_rebind <- function(text) {
  if_pattern <- "(^|[^A-Za-z0-9_@])[Ii][Ff]\\s*[][({]"
  extra <- character(0)
  from <- 1L
  k <- 0L
  repeat {
    rest <- substring(text, from)
    m <- regexpr(if_pattern, rest, perl = TRUE)
    if (m < 0L) {
      break
    }
    open <- from + m + attr(m, "match.length") - 2L
    close <- .match_bracket(text, open)
    if (is.na(close)) {
      break
    }
    inner <- substr(text, open + 1L, close - 1L)
    scan <- .tab_scan(inner)
    comma <- which(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote)
    cm <- regmatches(inner, regexec("^\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s+[Ii][Nn]\\s+([A-Za-z_][A-Za-z0-9_@]*)\\s*$", substr(inner, 1L, max(comma[1] - 1L, 0L))))[[1]]
    if (length(comma) > 0L && length(cm) > 0L) {
      k <- k + 1L
      idx <- cm[2]
      fresh <- paste0(idx, "@in", k)
      value <- gsub(
        paste0("(^|[^A-Za-z0-9_@])", idx, "([^A-Za-z0-9_@]|$)"),
        paste0("\\1", fresh, "\\2"),
        substring(inner, comma[1] + 1L),
        perl = TRUE,
        ignore.case = TRUE
      )
      value <- gsub(
        paste0("(^|[^A-Za-z0-9_@])", idx, "([^A-Za-z0-9_@]|$)"),
        paste0("\\1", fresh, "\\2"),
        value,
        perl = TRUE,
        ignore.case = TRUE
      )
      extra[[tolower(fresh)]] <- cm[3]
      text <- paste0(substr(text, 1L, open), substr(inner, 1L, comma[1]), value, substring(text, close))
    }
    from <- open + 1L
  }
  rebound <- list(text = text, extra = extra)
  return(rebound)
}
