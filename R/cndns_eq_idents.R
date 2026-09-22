#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.eq_idents <- function(entry) {
  idents <- purrr::map_chr(entry$quants, "idx")
  for (t in c(entry$lhs, entry$rhs)) {
    idents <- c(
      idents,
      purrr::map_chr(t$quants, "idx"),
      unlist(lapply(t$fac, .expr_idents)),
      if (!is.null(t$var)) {
        c(t$var$name, unlist(lapply(t$var$args, .expr_idents)))
      }
    )
  }
  idents <- unique(idents)
  return(idents)
}
