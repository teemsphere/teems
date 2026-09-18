#' @importFrom purrr map_chr
#'
# the # label # of a declaration, split off the statement remainder
# (declarations without one carry NA)
#' @keywords internal
#' @noRd
.label_tab_obj <- function(obj) {
  obj$remainder <- purrr::map_chr(
    obj$remainder,
    \(r) {
      if (!grepl("#", r)) {
        r <- paste(r, "# NA #")
      }
      return(r)
    }
  )

  obj$label <- purrr::map_chr(
    obj$remainder,
    \(r) {
      trimws(purrr::map_chr(strsplit(r, "#"), 2))
    }
  )

  obj$remainder <- purrr::map_chr(
    obj$remainder,
    \(r) {
      trimws(purrr::map_chr(strsplit(r, "#"), 1))
    }
  )
  return(obj)
}
