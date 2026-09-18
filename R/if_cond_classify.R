#' Classify an IF condition into the supported shapes.
#'
#' @keywords internal
#' @noRd
.classify_if_cond <- function(cond) {
  m <- regmatches(cond, regexec(
    "^([A-Za-z_][A-Za-z0-9_]*)\\s+[Ii][Nn]\\s+([A-Za-z_][A-Za-z0-9_]*)$",
    cond
  ))[[1]]
  if (length(m) > 0L) {
    cond <- list(kind = "in_set", idx = m[2], set = m[3])
    return(cond)
  }
  m <- regmatches(cond, regexec(
    '^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*"([^"]+)"$',
    cond
  ))[[1]]
  if (length(m) > 0L) {
    cond <- list(kind = "elem", idx = m[2], elem = m[3])
    return(cond)
  }
  ops <- c(
    eq = "=", ne = "<>", gt = ">", lt = "<", ge = ">=", le = "<=",
    "=" = "=", "<>" = "<>", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<="
  )
  m <- regmatches(cond, regexec(
    paste0(
      "^([A-Za-z_][A-Za-z0-9_]*(\\([^()]*\\))?)\\s*",
      "([Ee][Qq]|[Nn][Ee]|[Gg][Tt]|[Ll][Tt]|[Gg][Ee]|[Ll][Ee]|<=|>=|<>|=|<|>)\\s*",
      "([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)$"
    ),
    cond
  ))[[1]]
  if (length(m) > 0L) {
    cond <- list(
      kind = "cmp",
      ref = gsub("\\s", "", m[2]),
      op = ops[[tolower(m[4])]],
      num = m[5]
    )
    return(cond)
  }
  # general comparison of two arithmetic expressions (manual 11.4.5:
  # "conditions must be logical expressions ... typically comparison
  # operators"): carried by a synthesized helper coefficient, see
  # .if_expr_helper
  sp <- .split_comparison(cond)
  if (!is.null(sp)) {
    cond <- list(kind = "expr", lhs = sp$lhs, op = sp$op, rhs = sp$rhs)
    return(cond)
  }
  return(NULL)
}
