#' @keywords internal
#' @noRd
.canonical_mixed <- function(x,
                             ls_mixed) {
  if (length(x) == 0L || all(is.na(ls_mixed))) {
    return(x)
  }
  key <- tolower(ls_mixed)
  unique_key <- !key %in% key[duplicated(key)]
  hit <- match(tolower(x), key)
  swap <- !is.na(hit) & unique_key[hit] & !x %in% ls_mixed
  x[swap] <- ls_mixed[hit[swap]]
  year <- !x %in% ls_mixed & tolower(x) == "year"
  x[year] <- "Year"
  return(x)
}

#' @keywords internal
#' @noRd
.mixed_set <- function(x,
                       ls_mixed,
                       ls_upper) {
  set <- ls_upper[match(x, ls_mixed)]
  set[is.na(set)] <- .dock_tail(x[is.na(set)])
  return(set)
}
