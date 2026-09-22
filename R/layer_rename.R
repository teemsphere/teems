#' @keywords internal
#' @noRd
.layer_rename <- function(i_data, spec) {
  nm <- toupper(names(i_data))
  for (header in names(spec$rename_set)) {
    i <- match(header, nm)
    s <- i_data[[i]]
    class(s)[2] <- spec$rename_set[[header]]
    i_data[[i]] <- s
  }
  for (header in names(spec$rename_dim)) {
    i <- match(header, nm)
    a <- i_data[[i]]
    dn <- names(dimnames(a))
    if (!is.null(dn)) {
      r <- spec$rename_dim[[header]]
      dn[toupper(dn) == r[["from"]]] <- r[["to"]]
      names(dimnames(a)) <- dn
    }
    i_data[[i]] <- a
  }
  return(i_data)
}
