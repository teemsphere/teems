#' @keywords internal
#' @noRd
.cndns_share <- function(condense) {
  share <- paste0(
    format(round(100 * condense$elimination_share, 1), trim = TRUE),
    "%"
  )
  return(share)
}
