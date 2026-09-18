# term := factor (('*'|'/') factor)*
#' @keywords internal
#' @noRd
.pe_term <- function(st, var_lookup) {
  factors <- list(.pe_factor(st, var_lookup))
  ops <- "*"

  while (.pk(st) %in% c("*", "/")) {
    ops <- c(ops, .adv(st))
    factors[[length(factors) + 1L]] <- .pe_factor(st, var_lookup)
  }

  linear_at <- which(purrr::map_chr(factors, "kind") == "linear")

  if (length(linear_at) == 0L) {
    text <- factors[[1]]$text
    for (f in seq_along(factors)[-1]) {
      text <- paste0(text, ops[[f]], factors[[f]]$text)
    }
    node <- .coeff_node(text)
    return(node)
  }

  if (length(linear_at) > 1L) {
    stop("product of two variable-bearing expressions (nonlinear)",
         call. = FALSE)
  }

  if (ops[[linear_at]] %=% "/") {
    stop("division by a variable-bearing expression (nonlinear)",
         call. = FALSE)
  }

  terms <- factors[[linear_at]]$terms
  for (f in seq_along(factors)[-linear_at]) {
    terms <- lapply(terms, \(t) {
      t$fac <- c(t$fac, factors[[f]]$text)
      t$ops <- c(t$ops, ops[[f]])
      t
    })
  }
  node <- .linear_node(terms)
  return(node)
}
