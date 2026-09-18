#' The base-load and peak-load technologies of an electricity activity
#' list, split on the name suffix. Returns NULL when the suffix does not
#' partition the list, the caller aborting by name.
#'
#' @keywords internal
#' @noRd
.ep_load_split <- function(techs) {
  bl <- techs[grepl("bl$", techs, ignore.case = TRUE)]
  pl <- techs[grepl("[^b]p$", techs, ignore.case = TRUE)]
  if (length(bl) == 0L || length(pl) == 0L ||
    length(intersect(bl, pl)) > 0L ||
    !setequal(c(bl, pl), techs)) {
    return(NULL)
  }
  loads <- list(base = bl, peak = pl)
  return(loads)
}
