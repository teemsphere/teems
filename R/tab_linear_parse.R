# Linear-expression parser for TABLO equation definitions (condensation).
#
# A side of a linearized equation is flattened into a list of terms:
#   list(sign   = 1L | -1L,
#        quants = list(list(idx =, set =)),  # enclosing sums, outermost first
#        fac    = character(),               # coefficient factor texts
#        ops    = character(),               # "*" or "/" per factor
#        var    = list(name =, args =) | NULL)
# Subexpressions free of linear variables stay opaque single-factor texts
# so coefficient algebra is carried verbatim into rewritten equations.

#' @keywords internal
#' @noRd
.adv <- function(st) {
  tok <- .pk(st)
  st$pos <- st$pos + 1L
  return(tok)
}

#' @keywords internal
#' @noRd
.is_ident <- function(tok) {
  return(!is.na(tok) && grepl("^[A-Za-z_]", tok) && !grepl("^\"", tok))
}

#' @keywords internal
#' @noRd
.coeff_node <- function(text) {
  node <- list(kind = "coeff", text = text, terms = NULL)
  return(node)
}

#' @keywords internal
#' @noRd
.linear_node <- function(terms) {
  node <- list(kind = "linear", text = NULL, terms = terms)
  return(node)
}

# Parse one side of a linearized equation into flattened terms.
# `var_lookup` is a named list: tolower(variable name) -> canonical name.
# Errors are signalled with `stop()`; callers translate to cli aborts.
#' @keywords internal
#' @noRd
.parse_linear_side <- function(text, var_lookup) {
  st <- new.env(parent = emptyenv())
  st$tokens <- .tokenize_expr(text)
  st$pos <- 1L
  if (length(st$tokens) == 0L) {
    parsed <- list()
    return(parsed)
  }
  node <- .pe_expr(st, var_lookup)
  if (st$pos <= length(st$tokens)) {
    stop(paste0("trailing tokens starting at `", .pk(st), "`"), call. = FALSE)
  }
  parsed <- .coerce_terms(node)
  return(parsed)
}
