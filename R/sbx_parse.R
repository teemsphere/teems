#' @keywords internal
#' @noRd
.sbx_tokens <- function(text) {
  pattern <- paste(
    '"[^"]*"',
    "\\$?[A-Za-z_@][A-Za-z0-9_@]*",
    "[0-9]+\\.?[0-9]*(?:[eE][-+]?[0-9]+)?|\\.[0-9]+(?:[eE][-+]?[0-9]+)?",
    "<=|>=|<>|[-+*/^=<>(){}\\[\\],:]",
    "\\S",
    sep = "|"
  )
  tok <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  return(tok)
}

#' @keywords internal
#' @noRd
.sbx_parse <- function(text) {
  st <- new.env(parent = emptyenv())
  st$tok <- .sbx_tokens(text)
  st$i <- 1L
  node <- .sbx_or(st)
  if (st$i <= length(st$tok)) {
    stop("trailing", call. = FALSE)
  }
  return(node)
}

#' @keywords internal
#' @noRd
.sbx_peek <- function(st) {
  if (st$i > length(st$tok)) {
    return("")
  }
  return(st$tok[[st$i]])
}

#' @keywords internal
#' @noRd
.sbx_next <- function(st) {
  t <- .sbx_peek(st)
  if (!nzchar(t)) {
    stop("unexpected end", call. = FALSE)
  }
  st$i <- st$i + 1L
  return(t)
}

#' @keywords internal
#' @noRd
.sbx_expect <- function(st, what) {
  t <- .sbx_next(st)
  if (!t %in% what) {
    stop("expected ", what[1], call. = FALSE)
  }
  return(t)
}

#' @keywords internal
#' @noRd
.sbx_or <- function(st) {
  a <- .sbx_and(st)
  while (tolower(.sbx_peek(st)) == "or") {
    .sbx_next(st)
    a <- list(t = "bin", op = "or", a = a, b = .sbx_and(st))
  }
  return(a)
}

#' @keywords internal
#' @noRd
.sbx_and <- function(st) {
  a <- .sbx_not(st)
  while (tolower(.sbx_peek(st)) == "and") {
    .sbx_next(st)
    a <- list(t = "bin", op = "and", a = a, b = .sbx_not(st))
  }
  return(a)
}

#' @keywords internal
#' @noRd
.sbx_not <- function(st) {
  if (tolower(.sbx_peek(st)) == "not") {
    .sbx_next(st)
    return(list(t = "un", op = "not", x = .sbx_not(st)))
  }
  return(.sbx_cmp(st))
}

#' @keywords internal
#' @noRd
.sbx_cmp <- function(st) {
  a <- .sbx_add(st)
  ops <- c(
    "=" = "eq", "<>" = "ne", "<" = "lt", ">" = "gt", "<=" = "le", ">=" = "ge",
    eq = "eq", ne = "ne", lt = "lt", gt = "gt", le = "le", ge = "ge"
  )
  p <- tolower(.sbx_peek(st))
  if (p %in% names(ops)) {
    .sbx_next(st)
    a <- list(t = "bin", op = ops[[p]], a = a, b = .sbx_add(st))
  }
  return(a)
}

#' @keywords internal
#' @noRd
.sbx_add <- function(st) {
  a <- .sbx_mul(st)
  while (.sbx_peek(st) %in% c("+", "-")) {
    op <- .sbx_next(st)
    a <- list(t = "bin", op = op, a = a, b = .sbx_mul(st))
  }
  return(a)
}

#' @keywords internal
#' @noRd
.sbx_mul <- function(st) {
  a <- .sbx_unary(st)
  while (.sbx_peek(st) %in% c("*", "/")) {
    op <- .sbx_next(st)
    a <- list(t = "bin", op = op, a = a, b = .sbx_unary(st))
  }
  return(a)
}

#' @keywords internal
#' @noRd
.sbx_unary <- function(st) {
  if (.sbx_peek(st) %in% c("-", "+")) {
    op <- .sbx_next(st)
    x <- .sbx_unary(st)
    if (op == "+") {
      return(x)
    }
    return(list(t = "un", op = "neg", x = x))
  }
  return(.sbx_pow(st))
}

#' @keywords internal
#' @noRd
.sbx_pow <- function(st) {
  a <- .sbx_primary(st)
  if (.sbx_peek(st) == "^") {
    .sbx_next(st)
    a <- list(t = "bin", op = "^", a = a, b = .sbx_unary(st))
  }
  return(a)
}

#' @keywords internal
#' @noRd
.sbx_args <- function(st) {
  args <- list()
  if (.sbx_peek(st) %in% c(")", "]", "}")) {
    .sbx_next(st)
    return(args)
  }
  repeat {
    args[[length(args) + 1L]] <- .sbx_or(st)
    t <- .sbx_expect(st, c(",", ")", "]", "}"))
    if (t != ",") {
      break
    }
  }
  return(args)
}

#' @keywords internal
#' @noRd
.sbx_primary <- function(st) {
  t <- .sbx_next(st)
  if (t %in% c("(", "[", "{")) {
    x <- .sbx_or(st)
    .sbx_expect(st, c(")", "]", "}"))
    return(x)
  }
  if (grepl('^"', t)) {
    return(list(t = "str", v = tolower(gsub('"', "", t))))
  }
  if (grepl("^[0-9.]", t)) {
    return(list(t = "num", v = as.numeric(t)))
  }
  if (!grepl("^\\$?[A-Za-z_@]", t)) {
    stop("unexpected ", t, call. = FALSE)
  }
  low <- tolower(t)
  opens <- .sbx_peek(st) %in% c("(", "[", "{")
  if (low %in% c("sum", "prod", "maxs", "mins") && opens) {
    .sbx_next(st)
    idx <- .sbx_next(st)
    .sbx_expect(st, ",")
    set <- .sbx_next(st)
    cond <- NULL
    if (.sbx_peek(st) == ":") {
      .sbx_next(st)
      cond <- .sbx_or(st)
    }
    .sbx_expect(st, ",")
    body <- .sbx_or(st)
    .sbx_expect(st, c(")", "]", "}"))
    return(list(t = "agg", op = low, idx = idx, set = set, cond = cond, body = body))
  }
  if (low == "if" && opens) {
    .sbx_next(st)
    cond <- .sbx_or(st)
    .sbx_expect(st, ",")
    body <- .sbx_or(st)
    .sbx_expect(st, c(")", "]", "}"))
    return(list(t = "if", cond = cond, body = body))
  }
  if (low == "$pos" && opens) {
    .sbx_next(st)
    return(list(t = "pos", args = .sbx_args(st)))
  }
  fun <- c(
    "abs", "max", "min", "sqrt", "exp", "loge", "log10", "id01", "id0v", "round", "trunc0", "truncb",
    "normal", "cumnormal", "lognormal", "cumlognormal", "gperf", "gperfc"
  )
  if (low %in% fun && opens) {
    .sbx_next(st)
    return(list(t = "fun", name = low, args = .sbx_args(st)))
  }
  if (opens) {
    .sbx_next(st)
    return(list(t = "id", name = t, args = .sbx_args(st)))
  }
  return(list(t = "id", name = t, args = NULL))
}

#' @keywords internal
#' @noRd
.sbx_names <- function(node, bound = character(0)) {
  if (!is.list(node) || is.null(node$t)) {
    return(character(0))
  }
  rec <- \(x) .sbx_names(x, bound)
  out <- switch(node$t,
    id = c(
      if (is.null(node$args) && node$name %in% bound) character(0) else node$name,
      unlist(lapply(node$args, rec))
    ),
    agg = c(node$set, .sbx_names(node$cond, c(bound, node$idx)), .sbx_names(node$body, c(bound, node$idx))),
    `if` = c(rec(node$cond), rec(node$body)),
    pos = unlist(lapply(node$args, rec)),
    fun = unlist(lapply(node$args, rec)),
    un = rec(node$x),
    bin = c(rec(node$a), rec(node$b)),
    character(0)
  )
  return(out)
}
