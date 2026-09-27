#' @keywords internal
#' @noRd
.solver_uses_random <- function(paths) {
  if ("tab_path" %in% names(attributes(paths$cmf))) {
    tab <- readLines(attr(paths$cmf, "tab_path"))
  } else {
    tab <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = paths$cmf
    )
    tab <- readLines(tab)
  }
  uses_random <- any(grepl("(^|[^A-Za-z0-9_@])random\\s*[][({]", tab, ignore.case = TRUE, perl = TRUE))
  return(uses_random)
}
