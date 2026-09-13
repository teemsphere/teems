#' Canonical form of user-named set components: an element folds to the
#' lowercase every set carries, a subset name to its declared case, and
#' anything unrecognised is returned as given for the caller's check
#'
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
