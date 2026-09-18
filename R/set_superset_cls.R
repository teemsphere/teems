#' @keywords internal
#' @noRd
.set_superset_closure <- function(set_extract) {
  # closure[[D]] = every set that is a (transitive) subset of D
  nm <- tolower(set_extract$name)
  direct <- lapply(set_extract$subsets, \(s) {
    if (is.null(s) || all(is.na(s))) {
      character()
    } else {
      tolower(s[!is.na(s)])
    }
  })
  names(direct) <- nm
  closure <- direct
  for (d in nm) {
    seen <- character()
    queue <- direct[[d]]
    while (length(queue) > 0L) {
      s <- queue[[1]]
      queue <- queue[-1]
      if (s %in% seen) {
        next
      }
      seen <- c(seen, s)
      queue <- c(queue, direct[[s]] %|||% character())
    }
    closure[[d]] <- seen
  }
  return(closure)
}
