#' Split a condition at its top-level comparison operator. Symbol ops
#' (= <> < > <= >=) and word ops (eq ne lt gt le ge, identifier-
#' bounded); AND/OR/NOT compounds are not split (NULL). Returns
#' list(lhs, op, rhs) with op in symbol form, or NULL.
#'
#' @keywords internal
#' @noRd
.split_comparison <- function(cond) {
  scan <- .tab_scan(cond)
  chs <- scan$chs
  top <- scan$depth_before == 0L & !scan$in_quote
  n <- length(chs)
  if (grepl("(^|[^A-Za-z0-9_])(and|or|not)([^A-Za-z0-9_]|$)", cond, ignore.case = TRUE)) {
    return(NULL)
  }
  words <- c(eq = "=", ne = "<>", gt = ">", lt = "<", ge = ">=", le = "<=")
  is_id <- function(ch) {
    hit <- grepl("[A-Za-z0-9_]", ch)
    return(hit)
  }
  i <- 1L
  while (i <= n) {
    if (top[i]) {
      c1 <- chs[i]
      c2 <- if (i < n) {
        chs[i + 1L]
      } else {
        ""
      }
      if (c1 %in% c("<", ">")) {
        len <- if (c2 %in% c("=", ">")) {
          2L
        } else {
          1L
        }
        parts <- list(
          lhs = trimws(substr(cond, 1L, i - 1L)),
          op = substr(cond, i, i + len - 1L),
          rhs = trimws(substring(cond, i + len))
        )
        return(parts)
      }
      if (c1 == "=") {
        parts <- list(
          lhs = trimws(substr(cond, 1L, i - 1L)),
          op = "=",
          rhs = trimws(substring(cond, i + 1L))
        )
        return(parts)
      }
      w <- tolower(paste0(c1, c2))
      if (w %in% names(words) &&
        (i == 1L || !is_id(chs[i - 1L])) &&
        (i + 1L >= n || !is_id(chs[i + 2L]))) {
        parts <- list(
          lhs = trimws(substr(cond, 1L, i - 1L)),
          op = words[[w]],
          rhs = trimws(substring(cond, i + 2L))
        )
        return(parts)
      }
    }
    i <- i + 1L
  }
  return(NULL)
}
