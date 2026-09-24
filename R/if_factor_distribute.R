#' @keywords internal
#' @noRd
.distribute_if_factors <- function(terms,
                                   if_pattern) {
  sign <- character(0)
  body <- character(0)
  for (k in seq_along(terms$body)) {
    d <- .distribute_if_term(terms$body[k], if_pattern)
    if (is.null(d)) {
      sign <- c(sign, terms$sign[k])
      body <- c(body, terms$body[k])
      next
    }
    flip <- terms$sign[k] %=% "-"
    sign <- c(sign, ifelse(xor(d$sign == "-", flip), "-", "+"))
    body <- c(body, d$body)
  }
  terms <- list(sign = sign, body = body)
  return(terms)
}

#' @keywords internal
#' @noRd
.distribute_if_term <- function(b,
                                if_pattern) {
  if (!grepl(if_pattern, b) || !is.null(.parse_if_term(b))) {
    return(NULL)
  }
  scan <- .tab_scan(b)
  top <- scan$depth_before == 0L & !scan$in_quote
  ops <- which(top & scan$chs %in% c("*", "/"))
  if (length(ops) %=% 0L) {
    return(NULL)
  }
  last <- ops[length(ops)]
  first <- ops[1]
  tail <- trimws(substring(b, last + 1L))
  head <- trimws(substr(b, 1L, first - 1L))
  if (scan$chs[last] %=% "*" && grepl("^[][({]", tail) &&
    .group_spans(tail) && !grepl(if_pattern, substr(b, 1L, last - 1L))) {
    pre <- trimws(substr(b, 1L, last - 1L))
    inner <- substr(tail, 2L, nchar(tail) - 1L)
    wrap <- function(v) paste0(pre, " * [", v, "]")
  } else if (grepl("^[][({]", head) && .group_spans(head) &&
    !grepl(if_pattern, substring(b, first))) {
    post <- trimws(substring(b, first))
    inner <- substr(head, 2L, nchar(head) - 1L)
    wrap <- function(v) paste0("[", v, "] ", post)
  } else {
    return(NULL)
  }
  inner_terms <- .distribute_if_factors(.split_tab_terms(inner), if_pattern)
  out <- vapply(inner_terms$body, \(t) {
    p <- .parse_if_term(t)
    if (is.null(p)) {
      return(wrap(t))
    }
    return(sprintf("if[%s, %s]", p$cond, wrap(p$value)))
  }, character(1), USE.NAMES = FALSE)
  distributed <- list(sign = inner_terms$sign, body = out)
  return(distributed)
}

#' @keywords internal
#' @noRd
.group_spans <- function(s) {
  close <- .match_bracket(s, 1L)
  spans <- !is.na(close) && close %=% nchar(s)
  return(spans)
}
