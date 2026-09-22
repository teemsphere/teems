#' @keywords internal
#' @noRd
.split_tab_terms <- function(s) {
  scan <- .tab_scan(s)
  chs <- scan$chs
  n <- length(chs)
  is_pm <- chs %in% c("+", "-") & scan$depth_before == 0L & !scan$in_quote
  idx <- which(is_pm)
  expo <- idx > 2L & chs[pmax(idx - 1L, 1L)] %in% c("e", "E") &
    grepl("[0-9.]", chs[pmax(idx - 2L, 1L)])
  idx <- idx[!expo]
  first_char <- which(chs != " ")[1]
  lead_sign <- !is.na(first_char) && chs[first_char] %in% c("+", "-")
  idx <- idx[!idx %in% first_char]
  starts <- c(1L, idx)
  ends <- c(idx - 1L, n)
  body <- substring(s, starts, ends)
  sign <- c(
    if (lead_sign && chs[first_char] %=% "-") {
      "-"
    } else {
      "+"
    },
    chs[idx]
  )
  if (lead_sign) {
    body[1] <- sub("^\\s*[+-]", "", body[1])
  }
  body[-1] <- sub("^[+-]", "", body[-1])
  term <- list(sign = sign, body = trimws(body))
  return(term)
}
