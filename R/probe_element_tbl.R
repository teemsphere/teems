#' @description Parse solver-side element labels ("qo(svcs,reu)") into
#'   name plus element tuple. String split only: the names originate
#'   solver-side, so no TAB interpretation happens here.
#' @keywords internal
#' @noRd
.probe_element_tbl <- function(x) {
  if (is.null(x) || !length(x)) {
    tbl <- tibble::tibble(
      element = character(),
      name = character(),
      elements = list()
    )
    return(tbl)
  }
  name <- sub("\\(.*$", "", x)
  inner <- ifelse(grepl("(", x, fixed = TRUE),
    sub("^[^(]*\\(", "", sub("\\)$", "", x)),
    NA_character_
  )
  elements <- lapply(inner, \(i) {
    if (is.na(i)) {
      character()
    } else {
      strsplit(i, ",", fixed = TRUE)[[1]]
    }
  })
  tbl <- tibble::tibble(
    element = x,
    name = name,
    elements = elements
  )
  return(tbl)
}
