#' @keywords internal
#' @noRd
.probe_defects <- function(probe) {
  pieces <- list()
  for (pattern in c("structural", "realized")) {
    p <- probe[[pattern]]
    if (is.null(p)) {
      next
    }
    under <- .probe_element_tbl(p$under_determined_vars)
    over <- .probe_element_tbl(p$over_constrained_eqs)
    if (NROW(under)) {
      under$pattern <- pattern
      under$side <- "under_determined_variable"
      pieces <- c(pieces, list(under))
    }
    if (NROW(over)) {
      over$pattern <- pattern
      over$side <- "over_constrained_equation"
      pieces <- c(pieces, list(over))
    }
  }
  if (!length(pieces)) {
    defects <- tibble::tibble(
      pattern = character(),
      side = character(),
      element = character(),
      name = character(),
      elements = list()
    )
    return(defects)
  }
  out <- tibble::as_tibble(data.table::rbindlist(pieces))
  return(out[, c("pattern", "side", "element", "name", "elements")])
}
