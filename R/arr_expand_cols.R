#' @keywords internal
#' @noRd
.arr_expand_cols <- function(ulevs, out_sizes, keep, n_out) {
  cols <- vector("list", sum(keep))
  ci <- 0L
  for (k in seq_along(ulevs)) {
    if (!keep[k]) {
      next
    }
    ci <- ci + 1L
    each <- prod(out_sizes[seq_len(k - 1L)])
    idx <- rep(
      rep.int(seq_along(ulevs[[k]]), rep.int(as.integer(each), out_sizes[k])),
      length.out = n_out
    )
    cols[[ci]] <- ulevs[[k]][idx]
  }
  return(cols)
}