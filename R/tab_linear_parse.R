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
    stop(sprintf(model_err$linear_reason$trailing, .pk(st)), call. = FALSE)
  }
  parsed <- .coerce_terms(node)
  return(parsed)
}
