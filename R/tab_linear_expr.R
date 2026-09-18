# expr := ['+'|'-'] term (('+'|'-') term)*
#' @keywords internal
#' @noRd
.pe_expr <- function(st, var_lookup) {
  nodes <- list()
  signs <- integer()

  repeat {
    sign <- 1L
    while (.pk(st) %in% c("+", "-")) {
      if (.adv(st) %=% "-") {
        sign <- -sign
      }
    }
    node <- .pe_term(st, var_lookup)
    nodes[[length(nodes) + 1L]] <- node
    signs[[length(signs) + 1L]] <- sign
    if (.pk(st) %in% c("+", "-")) {
      next
    }
    break
  }

  if (all(purrr::map_chr(nodes, "kind") == "coeff")) {
    text <- ""
    for (n in seq_along(nodes)) {
      joint <- if (n == 1L) {
        ifelse(signs[[n]] == 1L, "", "-")
      } else {
        ifelse(signs[[n]] == 1L, " + ", " - ")
      }
      text <- paste0(text, joint, nodes[[n]]$text)
    }
    node <- .coeff_node(text)
    return(node)
  }

  terms <- list()
  for (n in seq_along(nodes)) {
    new <- .coerce_terms(nodes[[n]])
    if (signs[[n]] == -1L) {
      new <- .negate_terms(new)
    }
    terms <- c(terms, new)
  }
  node <- .linear_node(terms)
  return(node)
}
