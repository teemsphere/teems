#' @keywords internal
#' @noRd
.arr_codes <- function(dn, sets) {
  ulevs <- vector("list", length(dn))
  codes <- vector("list", length(dn))
  for (k in seq_along(dn)) {
    lev <- dn[[k]]
    nm <- names(dn)[k]
    if (!is.null(sets[[nm]])) {
      tab <- sets[[nm]]
      r_idx <- match(lev, tolower(tab[, 1][[1]]))
      .abort_unmapped(lev, r_idx, nm)
      lev <- tab[, 2][[1]][r_idx]
    }
    ulevs[[k]] <- unique(lev)
    codes[[k]] <- match(lev, ulevs[[k]]) - 1L
  }
  codes <- list(ulevs = ulevs, codes = codes)
  return(codes)
}