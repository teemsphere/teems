#' @keywords internal
#' @noRd
NULL

#' @keywords internal
#' @noRd
.parse_set_builder <- function(d) {
  d <- trimws(sub("^\\s*=\\s*", "", d))
  if (!startsWith(d, "(")) {
    return(NULL)
  }
  close <- .match_bracket(d, 1L)
  if (is.na(close) || nzchar(trimws(substring(d, close + 1L)))) {
    return(NULL)
  }
  inner <- substr(d, 2L, close - 1L)
  m <- regmatches(inner, regexec(
    "^\\s*[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*:(.*)$",
    inner
  ))[[1]]
  if (length(m) == 0L) {
    return(NULL)
  }
  idx <- m[2]
  src <- m[3]
  cond <- trimws(m[4])

  scan <- .tab_scan(cond)
  chs <- scan$chs
  top <- scan$depth_before == 0L & !scan$in_quote
  op_at <- NA_integer_
  op_len <- 0L
  n <- length(chs)
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
        op_at <- i
        op_len <- if (c2 %in% c("=", ">")) {
          2L
        } else {
          1L
        }
        break
      }
      if (c1 == "=") {
        op_at <- i
        op_len <- 1L
        break
      }
      if (c1 == " " && i + 3L <= n &&
        tolower(paste0(chs[i + 1L], chs[i + 2L])) %in%
          c("ne", "eq", "gt", "lt", "ge", "le") &&
        chs[i + 3L] == " ") {
        op_at <- i + 1L
        op_len <- 2L
        break
      }
    }
    i <- i + 1L
  }
  if (is.na(op_at)) {
    return(NULL)
  }
  op <- tolower(substr(cond, op_at, op_at + op_len - 1L))
  ops <- c(
    "=" = "eq", "<>" = "ne", ">" = "gt", "<" = "lt", ">=" = "ge", "<=" = "le",
    eq = "eq", ne = "ne", gt = "gt", lt = "lt", ge = "ge", le = "le"
  )
  if (!op %in% names(ops)) {
    return(NULL)
  }
  const <- trimws(substring(cond, op_at + op_len))
  if (!grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", const)) {
    return(NULL)
  }
  operand <- trimws(substr(cond, 1L, op_at - 1L))
  out <- list(
    idx = idx, src = src, cond = cond,
    op = ops[[op]], const = as.numeric(const), operand = operand
  )

  if (grepl("^[Ss][Uu][Mm]\\s*[][({]", operand)) {
    open <- regexpr("[][({]", operand)
    cl <- .match_bracket(operand, open)
    if (is.na(cl) || nzchar(trimws(substring(operand, cl + 1L)))) {
      return(NULL)
    }
    body <- substr(operand, open + 1L, cl - 1L)
    sm <- regmatches(body, regexec(paste0(
      "^\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*:\\s*",
      "([A-Za-z_][A-Za-z0-9_@]*)\\s*[[({]\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*[])}]\\s*=\\s*",
      "([A-Za-z_][A-Za-z0-9_@]*)\\s*,\\s*",
      "([A-Za-z_][A-Za-z0-9_@]*)\\s*[[({]\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*[])}]\\s*$"
    ), body))[[1]]
    if (length(sm) == 0L) {
      return(NULL)
    }
    j <- sm[2]
    if (tolower(sm[5]) != tolower(j) || tolower(sm[6]) != tolower(idx) ||
      tolower(sm[8]) != tolower(j)) {
      return(NULL)
    }
    out$form <- "mapsum"
    out$sum_idx <- j
    out$sum_set <- sm[3]
    out$map <- sm[4]
    out$coef <- sm[7]
    return(out)
  }

  cm <- regmatches(operand, regexec(
    "^([A-Za-z_][A-Za-z0-9_@]*)\\s*[[({](.*)[])}]\\s*$",
    operand
  ))[[1]]
  if (length(cm) == 0L) {
    return(NULL)
  }
  args <- trimws(strsplit(cm[3], ",", fixed = TRUE)[[1]])
  if (length(args) == 0L || any(!nzchar(args))) {
    return(NULL)
  }
  is_idx <- tolower(args) == tolower(idx)
  is_quoted <- grepl('^"[^"]+"$', args)
  if (sum(is_idx) != 1L || !all(is_idx | is_quoted)) {
    return(NULL)
  }
  out$form <- "coef"
  out$coef <- cm[2]
  out$args <- args
  out$loop_dim <- which(is_idx)
  return(out)
}

#' @keywords internal
#' @noRd
.is_set_builder <- function(d) {
  return(length(d) == 1L && !is.na(d) &&
    grepl("^\\s*=\\s*\\(\\s*all\\s*,", d, ignore.case = TRUE))
}

#' @keywords internal
#' @noRd
.sb_op_test <- function(v, op, c) {
  hit <- switch(op,
    eq = v == c, ne = v != c, gt = v > c, lt = v < c, ge = v >= c, le = v <= c
  )
  return(hit)
}
