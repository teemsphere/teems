# factor := ['+'|'-'] (NUMBER | ELEMENT | ref | sum | '(' expr ')' | '[' expr ']')
#' @keywords internal
#' @noRd
.pe_factor <- function(st, var_lookup) {
  tok <- .pk(st)

  if (tok %in% c("+", "-")) {
    .adv(st)
    node <- .pe_factor(st, var_lookup)
    if (tok %=% "-") {
      if (node$kind %=% "coeff") {
        node$text <- paste0("-", node$text)
      } else {
        node$terms <- .negate_terms(node$terms)
      }
    }
    return(node)
  }

  if (tok %in% c("(", "[")) {
    .adv(st)
    close <- ifelse(tok %=% "(", ")", "]")
    node <- .pe_expr(st, var_lookup)
    .expect(st, close)
    if (node$kind %=% "coeff") {
      node$text <- paste0(tok, node$text, close)
    }
    return(node)
  }

  if (is.na(tok)) {
    stop("unexpected end of expression", call. = FALSE)
  }

  if (grepl("^\"", tok) || grepl("^[0-9.]", tok)) {
    .adv(st)
    node <- .coeff_node(tok)
    return(node)
  }

  if (!.is_ident(tok)) {
    stop(paste0("unexpected token `", tok, "`"), call. = FALSE)
  }

  .adv(st)

  if (tolower(tok) %=% "sum" && .pk(st) %in% c("{", "(")) {
    open <- .adv(st)
    close <- ifelse(open %=% "{", "}", ")")
    idx <- .adv(st)
    if (!.is_ident(idx)) {
      stop("malformed sum index", call. = FALSE)
    }
    .expect(st, ",")
    set <- .adv(st)
    if (!.is_ident(set)) {
      stop("malformed sum set", call. = FALSE)
    }
    # `sum{j,S: COND, expr}` -- the condition ranges over set elements
    # only (11.9), so substitution never touches it: it is captured
    # verbatim and serialized back onto the sum it came from
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

  # TABLO takes `(` and `]` as interchangeable with `[` and `)`, in a
  # reference's arguments as much as in a grouping: GTAP-E writes the
  # intrinsic as ID01[VXW(c,r)]. The brackets actually used are kept so
  # a coefficient factor serializes back as it was written.
  args <- NULL
  open <- "("
  if (.pk(st) %in% c("(", "[")) {
    open <- .adv(st)
    args <- .pe_args(st)
  }
  close <- ifelse(open %=% "[", "]", ")")

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
      stop(paste0("variable reference inside the arguments of `", tok, "`"),
           call. = FALSE)
    }
  }
  node <- .coeff_node(paste0(tok, open, paste(args, collapse = ","), close))
  return(node)
}
