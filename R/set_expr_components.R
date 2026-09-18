#' @importFrom purrr map2 map_chr
#'
# the operator and the (up to two) named operands of a set expression,
# recorded on the declaration; a builder's source set is its operand
#' @keywords internal
#' @noRd
.set_expr_components <- function(sets,
                                 is_expr,
                                 is_builder) {
  expr_info <- purrr::map2(sets$definition, is_expr, \(d, e) {
    if (isTRUE(e)) {
      .set_expr_info(d)
    } else {
      NA
    }
  })

  sets$operator <- purrr::map_chr(expr_info, \(fo) {
    if (!is.list(fo) || length(fo$ops) %=% 0L) {
      return(NA_character_)
    }
    switch(fo$ops[1], "^" = "union", "&" = "intersect", fo$ops[1])
  })

  sets$comp1 <- purrr::map_chr(expr_info, \(fo) {
    if (is.list(fo) && length(fo$named) >= 1L) {
      fo$named[1]
    } else {
      NA_character_
    }
  })
  # a builder's source set is its (implied) superset
  for (i in which(is_builder)) {
    sets$comp1[i] <- .parse_set_builder(sets$definition[[i]])$src
  }
  sets$comp2 <- purrr::map_chr(expr_info, \(fo) {
    if (is.list(fo) && length(fo$named) >= 2L) {
      fo$named[2]
    } else {
      NA_character_
    }
  })
  components <- list(sets = sets, expr_info = expr_info)
  return(components)
}
