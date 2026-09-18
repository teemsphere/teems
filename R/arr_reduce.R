# marginal sum of a numeric array over the dims named in `keep`,
# returned as a data.table with lowercased level columns; used to build
# parameter weights without expanding the source array
#' @keywords internal
#' @noRd
.reduce_array <- function(arr,
                          keep) {
  dn <- lapply(dimnames(arr), tolower)
  nms <- names(dn)
  keep_dim <- nms %in% keep

  ulevs <- vector("list", length(dn))
  codes <- vector("list", length(dn))
  for (k in seq_along(dn)) {
    if (keep_dim[k]) {
      ulevs[[k]] <- unique(dn[[k]])
      codes[[k]] <- match(dn[[k]], ulevs[[k]]) - 1L
    } else {
      ulevs[[k]] <- NA_character_
      codes[[k]] <- integer(length(dn[[k]]))
    }
  }
  out_sizes <- vapply(ulevs, length, integer(1))
  val <- agg_array_sum(arr, codes, out_sizes)

  cols <- .arr_expand_cols(
    ulevs = ulevs,
    out_sizes = out_sizes,
    keep = keep_dim,
    n_out = length(val)
  )
  names(cols) <- nms[keep_dim]
  dt <- data.table::setDT(c(cols, list(Value = val)))
  return(dt)
}
