#' @keywords internal
#' @noRd
.canonical_ele <- function(x,
                           ele,
                           subsets) {
  if (!is.character(x)) {
    return(x)
  }
  low <- tolower(x)
  ss_idx <- match(toupper(x), toupper(subsets))
  out <- ifelse(low %in% ele, low, ifelse(is.na(ss_idx), x, subsets[ss_idx]))
  x[] <- out
  return(x)
}
