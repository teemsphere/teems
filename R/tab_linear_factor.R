#' @keywords internal
#' @noRd
.pe_factor <- function(st, var_lookup) {
  node <- .pe_primary(st, var_lookup)
  while (.pk(st) %=% "^") {
    .adv(st)
    ex <- .pe_primary(st, var_lookup)
    if (!node$kind %=% "coeff" || !ex$kind %=% "coeff") {
      stop(model_err$linear_reason$power, call. = FALSE)
    }
    node <- .coeff_node(paste0(node$text, "^", ex$text))
  }
  return(node)
}

#' @keywords internal
#' @noRd
.pe_primary <- function(st, var_lookup) {
  tok <- .pk(st)

  if (tok %in% c("+", "-")) {
    .adv(st)
    node <- .pe_primary(st, var_lookup)
    if (tok %=% "-") {
      if (node$kind %=% "coeff") {
        node$text <- paste0("-", node$text)
      } else {
        node$terms <- .negate_terms(node$terms)
      }
    }
    return(node)
  }

  if (tok %in% c("(", "[", "{")) {
    .adv(st)
    close <- switch(tok, "(" = ")", "[" = "]", "{" = "}")
    node <- .pe_expr(st, var_lookup)
    .expect(st, close)
    if (node$kind %=% "coeff") {
      node$text <- paste0(tok, node$text, close)
    }
    return(node)
  }

  if (is.na(tok)) {
    stop(model_err$linear_reason$unexpected_end, call. = FALSE)
  }

  if (grepl("^\"", tok) || grepl("^[0-9.]", tok)) {
    .adv(st)
    node <- .coeff_node(tok)
    return(node)
  }

  if (!.is_ident(tok)) {
    stop(sprintf(model_err$linear_reason$unexpected_token, tok), call. = FALSE)
  }

  .adv(st)

  if (tolower(tok) %=% "if" && .pk(st) %in% c("{", "(", "[")) {
    open <- .adv(st)
    close <- switch(open, "{" = "}", "(" = ")", "[" = "]")
    cond <- .pe_if_cond(st, var_lookup)
    .expect(st, ",")
    node <- .pe_expr(st, var_lookup)
    .expect(st, close)
    if (node$kind %=% "coeff") {
      node <- .coeff_node(paste0("IF[", cond, ", ", node$text, "]"))
      return(node)
    }
    node$terms <- lapply(node$terms, \(t) {
      t$fac <- c(t$fac, paste0("IF[", cond, ", 1]"))
      t$ops <- c(t$ops, "*")
      t
    })
    return(node)
  }

  if (tolower(tok) %=% "sum" && .pk(st) %in% c("{", "(", "[")) {
    open <- .adv(st)
    close <- switch(open, "{" = "}", "(" = ")", "[" = "]")
    idx <- .adv(st)
    if (!.is_ident(idx)) {
      stop(model_err$linear_reason$sum_index, call. = FALSE)
    }
    .expect(st, ",")
    set <- .adv(st)
    if (!.is_ident(set)) {
      stop(model_err$linear_reason$sum_set, call. = FALSE)
    }
    cond <- .pe_sum_cond(st)
    .expect(st, ",")
    node <- .pe_expr(st, var_lookup)
    .expect(st, close)
    if (node$kind %=% "coeff") {
      node <- .coeff_node(paste0("sum{", idx, ",", set, cond, ", ",
                                node$text, "}"))
      return(node)
    }
    node$terms <- lapply(node$terms, \(t) {
      t$quants <- c(list(list(idx = idx, set = set, cond = cond)), t$quants)
      t
    })
    return(node)
  }

  args <- NULL
  open <- "("
  reduction <- tolower(tok) %in% c("prod", "maxs", "mins")
  if (.pk(st) %in% c("(", "[") || (reduction && .pk(st) %=% "{")) {
    open <- .adv(st)
    args <- .pe_args(st)
  }
  close <- switch(open, "[" = "]", "{" = "}", ")")

  canonical <- var_lookup[[tolower(tok)]]
  if (!is.null(canonical)) {
    node <- .linear_node(list(.new_term(
      var = list(name = canonical, args = args %|||% character())
    )))
    return(node)
  }

  if (is.null(args)) {
    node <- .coeff_node(tok)
    return(node)
  }

  for (a in args) {
    arg_idents <- .expr_idents(a)
    if (any(tolower(arg_idents) %in% names(var_lookup))) {
      stop(sprintf(model_err$linear_reason$var_in_args, tok), call. = FALSE)
    }
  }
  node <- .coeff_node(paste0(tok, open, paste(args, collapse = ","), close))
  return(node)
}
